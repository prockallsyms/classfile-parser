use crate::code_attribute::Instruction;
use crate::decompile::descriptor::{parse_method_descriptor, JvmType};
use crate::decompile::util::instruction_byte_size;
use crate::ClassFile;

use super::ast::*;
use super::stackmap::{FrameTracker, VType};
use super::CompileError;

/// Tracks local variable allocation.
struct LocalAllocator {
    /// (name, type, slot)
    locals: Vec<(String, TypeName, u16)>,
    next_slot: u16,
}

impl LocalAllocator {
    fn new(is_static: bool, method_descriptor: &str) -> Result<Self, CompileError> {
        let mut next_slot: u16 = 0;
        let mut locals = Vec::new();

        if !is_static {
            locals.push(("this".into(), TypeName::Class("this".into()), 0));
            next_slot = 1;
        }

        // Parse method descriptor to pre-allocate parameter slots
        let (params, _) = parse_method_descriptor(method_descriptor).ok_or_else(|| {
            CompileError::CodegenError {
                message: format!("invalid method descriptor: {}", method_descriptor),
            }
        })?;

        for (i, param) in params.iter().enumerate() {
            let ty = jvm_type_to_type_name(param);
            let slot = next_slot;
            locals.push((format!("__param{}", i), ty, slot));
            next_slot += if param.is_wide() { 2 } else { 1 };
        }

        Ok(LocalAllocator { locals, next_slot })
    }

    fn allocate(&mut self, name: &str, ty: &TypeName) -> u16 {
        let slot = self.next_slot;
        let width = type_slot_width(ty);
        self.locals.push((name.to_string(), ty.clone(), slot));
        self.next_slot += width;
        slot
    }

    fn find(&self, name: &str) -> Option<(u16, &TypeName)> {
        // Search from the end to support shadowing
        for (n, ty, slot) in self.locals.iter().rev() {
            if n == name {
                return Some((*slot, ty));
            }
        }
        None
    }

    fn max_locals(&self) -> u16 {
        self.next_slot
    }

    /// Get current locals as VType array for StackMapTable generation.
    fn current_locals_vtypes(&self) -> Vec<VType> {
        let mut vtypes = vec![VType::Top; self.next_slot as usize];
        for (_, ty, slot) in &self.locals {
            vtypes[*slot as usize] = type_name_to_vtype(ty);
            if type_slot_width(ty) == 2 {
                if (*slot as usize + 1) < vtypes.len() {
                    vtypes[*slot as usize + 1] = VType::Top;
                }
            }
        }
        // Trim trailing Top values
        while vtypes.last() == Some(&VType::Top) {
            vtypes.pop();
        }
        vtypes
    }
}

struct BreakableContext {
    break_label: usize,
    is_loop: bool,
    continue_label: Option<usize>,
}

enum SwitchPatchKind {
    Table {
        low: i32,
        high: i32,
        /// Labels for offsets[0..=(high-low)], then default_label
        case_labels: Vec<usize>,
        default_label: usize,
    },
    Lookup {
        /// (match_value, label)
        pairs: Vec<(i32, usize)>,
        default_label: usize,
    },
}

struct SwitchPatch {
    instr_idx: usize,
    kind: SwitchPatchKind,
}

struct PendingExceptionEntry {
    start_label: usize,
    end_label: usize,
    handler_label: usize,
    catch_type: u16,
}

pub struct CodeGenerator<'a> {
    class_file: &'a mut ClassFile,
    instructions: Vec<Instruction>,
    locals: LocalAllocator,
    labels: Vec<Option<usize>>,     // label_id → instruction index (None = unresolved)
    patches: Vec<(usize, usize)>,   // (instruction_index, target_label_id)
    switch_patches: Vec<SwitchPatch>,
    breakable_stack: Vec<BreakableContext>,
    pending_exceptions: Vec<PendingExceptionEntry>,
    is_static: bool,
    return_type: JvmType,
    frame_tracker: Option<FrameTracker>,
    /// Labels that are exception handler entry points: (label_id, exception_vtype, locals_at_try_start).
    exception_handler_labels: Vec<(usize, VType, Vec<VType>)>,
    /// Labels whose frame locals should be overridden (not taken from current allocator).
    label_locals_override: Vec<(usize, Vec<VType>)>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(
        class_file: &'a mut ClassFile,
        is_static: bool,
        method_descriptor: &str,
    ) -> Result<Self, CompileError> {
        Self::new_with_options(class_file, is_static, method_descriptor, false)
    }

    pub fn new_with_options(
        class_file: &'a mut ClassFile,
        is_static: bool,
        method_descriptor: &str,
        generate_stack_map_table: bool,
    ) -> Result<Self, CompileError> {
        let locals = LocalAllocator::new(is_static, method_descriptor)?;
        let (params, ret) = parse_method_descriptor(method_descriptor).ok_or_else(|| {
            CompileError::CodegenError {
                message: format!("invalid method descriptor: {}", method_descriptor),
            }
        })?;

        let frame_tracker = if generate_stack_map_table {
            // Build initial locals VTypes from method descriptor
            let mut initial = Vec::new();
            if !is_static {
                // 'this' reference — resolve the class
                initial.push(VType::Object(class_file.this_class));
            }
            for param in &params {
                initial.push(jvm_type_to_vtype(param));
                if param.is_wide() {
                    initial.push(VType::Top);
                }
            }
            Some(FrameTracker::new(initial))
        } else {
            None
        };

        Ok(CodeGenerator {
            class_file,
            instructions: Vec::new(),
            locals,
            labels: Vec::new(),
            patches: Vec::new(),
            switch_patches: Vec::new(),
            breakable_stack: Vec::new(),
            pending_exceptions: Vec::new(),
            is_static,
            return_type: ret,
            frame_tracker,
            exception_handler_labels: Vec::new(),
            label_locals_override: Vec::new(),
        })
    }

    fn new_label(&mut self) -> usize {
        let id = self.labels.len();
        self.labels.push(None);
        id
    }

    fn bind_label(&mut self, label_id: usize) {
        let instr_idx = self.instructions.len();
        self.labels[label_id] = Some(instr_idx);

        // Record frame snapshot for StackMapTable generation
        if self.frame_tracker.is_some() {
            // Check if this label is an exception handler entry point
            let exception_info = self
                .exception_handler_labels
                .iter()
                .find(|(lid, _, _)| *lid == label_id)
                .map(|(_, vtype, saved_locals)| (vtype.clone(), saved_locals.clone()));

            let (locals, stack) = if let Some((vtype, saved_locals)) = exception_info {
                // Exception handlers use the locals from try-start, not current allocator state
                (saved_locals, vec![vtype])
            } else {
                // Check for explicit locals override (e.g., merge points after try-catch)
                let overridden_locals = self
                    .label_locals_override
                    .iter()
                    .find(|(lid, _)| *lid == label_id)
                    .map(|(_, locals)| locals.clone());
                let locals = overridden_locals.unwrap_or_else(|| self.locals.current_locals_vtypes());
                (locals, Vec::new())
            };

            // We need to compute the bytecode offset for this instruction index.
            // Since instructions haven't been patched yet, compute from current instructions.
            let offset = compute_byte_offset_at(&self.instructions, instr_idx);

            if let Some(ref mut tracker) = self.frame_tracker {
                tracker.record_frame(offset, locals, stack);
            }
        }
    }

    fn emit(&mut self, instr: Instruction) -> usize {
        let idx = self.instructions.len();
        self.instructions.push(instr);
        idx
    }

    fn emit_branch(&mut self, instr_fn: fn(i16) -> Instruction, target_label: usize) {
        let idx = self.emit(instr_fn(0)); // placeholder
        self.patches.push((idx, target_label));
    }

    fn emit_goto(&mut self, target_label: usize) {
        self.emit_branch(Instruction::Goto, target_label);
    }

    /// Resolve all branch patches using byte addresses.
    fn resolve_patches(&mut self) -> Result<(), CompileError> {
        // Compute byte addresses for each instruction
        let addresses = compute_byte_addresses(&self.instructions);

        // Compute end address (address after last instruction)
        let end_addr = if self.instructions.is_empty() {
            0i32
        } else {
            let last = *addresses.last().unwrap();
            (last + instruction_byte_size(self.instructions.last().unwrap(), last as u32)) as i32
        };

        for &(instr_idx, label_id) in &self.patches {
            let source_addr = addresses[instr_idx] as i32;
            let target_addr = resolve_label_addr(label_id, &self.labels, &addresses, end_addr)?;
            let offset = target_addr - source_addr;
            let offset16 = offset as i16;

            self.instructions[instr_idx] = patch_branch_offset(&self.instructions[instr_idx], offset16)?;
        }

        // Resolve switch patches
        for patch in &self.switch_patches {
            let source_addr = addresses[patch.instr_idx] as i32;
            match &patch.kind {
                SwitchPatchKind::Table { low, high, case_labels, default_label } => {
                    let default_offset = resolve_label_addr(*default_label, &self.labels, &addresses, end_addr)? - source_addr;
                    let mut offsets = Vec::new();
                    for label_id in case_labels {
                        let addr = resolve_label_addr(*label_id, &self.labels, &addresses, end_addr)?;
                        offsets.push(addr - source_addr);
                    }
                    self.instructions[patch.instr_idx] = Instruction::Tableswitch {
                        default: default_offset,
                        low: *low,
                        high: *high,
                        offsets,
                    };
                }
                SwitchPatchKind::Lookup { pairs, default_label } => {
                    let default_offset = resolve_label_addr(*default_label, &self.labels, &addresses, end_addr)? - source_addr;
                    let mut resolved_pairs = Vec::new();
                    for (value, label_id) in pairs {
                        let addr = resolve_label_addr(*label_id, &self.labels, &addresses, end_addr)?;
                        resolved_pairs.push((*value, addr - source_addr));
                    }
                    self.instructions[patch.instr_idx] = Instruction::Lookupswitch {
                        default: default_offset,
                        npairs: resolved_pairs.len() as u32,
                        pairs: resolved_pairs,
                    };
                }
            }
        }

        Ok(())
    }

    pub fn generate_body(&mut self, stmts: &[CStmt]) -> Result<(), CompileError> {
        for stmt in stmts {
            self.gen_stmt(stmt)?;
        }
        // If the method returns void, ensure there's a trailing return.
        // Any label pointing to `instructions.len()` (end of code) needs a
        // valid instruction there, so always emit Return for void methods
        // when the last emitted instruction isn't already a return/throw,
        // OR when there are labels that point to the end of instructions.
        if self.return_type == JvmType::Void {
            let has_label_at_end = self
                .labels
                .iter()
                .any(|l| *l == Some(self.instructions.len()));
            let needs_return = self.instructions.is_empty()
                || has_label_at_end
                || !matches!(
                    self.instructions.last(),
                    Some(Instruction::Return)
                        | Some(Instruction::Ireturn)
                        | Some(Instruction::Lreturn)
                        | Some(Instruction::Freturn)
                        | Some(Instruction::Dreturn)
                        | Some(Instruction::Areturn)
                        | Some(Instruction::Athrow)
                );
            if needs_return {
                self.emit(Instruction::Return);
            }
        }
        Ok(())
    }

    pub fn finish(mut self) -> Result<super::GeneratedCode, CompileError> {
        self.resolve_patches()?;
        let max_stack = super::stack_calc::compute_max_stack(&self.instructions);
        let max_locals = self.locals.max_locals();
        let exception_table = self.build_exception_table()?;
        let stack_map_table = self.frame_tracker.take().and_then(|t| t.build());
        Ok(super::GeneratedCode {
            instructions: self.instructions,
            max_stack,
            max_locals,
            exception_table,
            stack_map_table,
        })
    }

    fn build_exception_table(&self) -> Result<Vec<crate::attribute_info::ExceptionEntry>, CompileError> {
        use crate::attribute_info::ExceptionEntry;
        let addresses = compute_byte_addresses(&self.instructions);
        let end_addr = {
            if self.instructions.is_empty() {
                0u16
            } else {
                let last = addresses.last().copied().unwrap_or(0);
                let last_instr = &self.instructions[self.instructions.len() - 1];
                (last + instruction_byte_size(last_instr, last as u32)) as u16
            }
        };

        let mut entries = Vec::new();
        for pending in &self.pending_exceptions {
            let start_instr = self.labels[pending.start_label].ok_or_else(|| CompileError::CodegenError {
                message: "unresolved exception start label".into(),
            })?;
            let end_instr = self.labels[pending.end_label].ok_or_else(|| CompileError::CodegenError {
                message: "unresolved exception end label".into(),
            })?;
            let handler_instr = self.labels[pending.handler_label].ok_or_else(|| CompileError::CodegenError {
                message: "unresolved exception handler label".into(),
            })?;

            let start_pc = if start_instr < addresses.len() {
                addresses[start_instr] as u16
            } else {
                end_addr
            };
            let end_pc = if end_instr < addresses.len() {
                addresses[end_instr] as u16
            } else {
                end_addr
            };
            let handler_pc = if handler_instr < addresses.len() {
                addresses[handler_instr] as u16
            } else {
                end_addr
            };

            entries.push(ExceptionEntry {
                start_pc,
                end_pc,
                handler_pc,
                catch_type: pending.catch_type,
            });
        }
        Ok(entries)
    }

    // --- Statement codegen ---

    fn gen_stmt(&mut self, stmt: &CStmt) -> Result<(), CompileError> {
        match stmt {
            CStmt::LocalDecl { ty, name, init } => {
                let slot = self.locals.allocate(name, ty);
                if let Some(expr) = init {
                    self.gen_expr(expr)?;
                    self.emit_store(ty, slot);
                }
                Ok(())
            }
            CStmt::ExprStmt(expr) => {
                self.gen_expr(expr)?;
                // Pop the value if the expression leaves one on the stack
                if self.expr_leaves_value(expr) {
                    self.emit(Instruction::Pop);
                }
                Ok(())
            }
            CStmt::Return(None) => {
                self.emit(Instruction::Return);
                Ok(())
            }
            CStmt::Return(Some(expr)) => {
                self.gen_expr(expr)?;
                let ret_instr = match &self.return_type {
                    JvmType::Int | JvmType::Boolean | JvmType::Byte | JvmType::Char | JvmType::Short => {
                        Instruction::Ireturn
                    }
                    JvmType::Long => Instruction::Lreturn,
                    JvmType::Float => Instruction::Freturn,
                    JvmType::Double => Instruction::Dreturn,
                    JvmType::Reference(_) | JvmType::Array(_) | JvmType::Null => Instruction::Areturn,
                    JvmType::Void => Instruction::Return,
                    JvmType::Unknown => Instruction::Areturn,
                };
                self.emit(ret_instr);
                Ok(())
            }
            CStmt::If {
                condition,
                then_body,
                else_body,
            } => {
                let false_label = self.new_label();
                self.gen_condition(condition, false_label, false)?;
                for s in then_body {
                    self.gen_stmt(s)?;
                }
                if let Some(else_stmts) = else_body {
                    let end_label = self.new_label();
                    self.emit_goto(end_label);
                    self.bind_label(false_label);
                    for s in else_stmts {
                        self.gen_stmt(s)?;
                    }
                    self.bind_label(end_label);
                } else {
                    self.bind_label(false_label);
                }
                Ok(())
            }
            CStmt::While { condition, body } => {
                let top_label = self.new_label();
                let end_label = self.new_label();
                self.breakable_stack.push(BreakableContext {
                    break_label: end_label,
                    is_loop: true,
                    continue_label: Some(top_label),
                });
                self.bind_label(top_label);
                self.gen_condition(condition, end_label, false)?;
                for s in body {
                    self.gen_stmt(s)?;
                }
                self.emit_goto(top_label);
                self.bind_label(end_label);
                self.breakable_stack.pop();
                Ok(())
            }
            CStmt::For {
                init,
                condition,
                update,
                body,
            } => {
                if let Some(init_stmt) = init {
                    self.gen_stmt(init_stmt)?;
                }
                let top_label = self.new_label();
                let update_label = self.new_label();
                let end_label = self.new_label();
                self.breakable_stack.push(BreakableContext {
                    break_label: end_label,
                    is_loop: true,
                    continue_label: Some(update_label),
                });
                self.bind_label(top_label);
                if let Some(cond) = condition {
                    self.gen_condition(cond, end_label, false)?;
                }
                for s in body {
                    self.gen_stmt(s)?;
                }
                self.bind_label(update_label);
                if let Some(upd) = update {
                    self.gen_stmt(upd)?;
                }
                self.emit_goto(top_label);
                self.bind_label(end_label);
                self.breakable_stack.pop();
                Ok(())
            }
            CStmt::Block(stmts) => {
                for s in stmts {
                    self.gen_stmt(s)?;
                }
                Ok(())
            }
            CStmt::Throw(expr) => {
                self.gen_expr(expr)?;
                self.emit(Instruction::Athrow);
                Ok(())
            }
            CStmt::Break => {
                let label = self
                    .breakable_stack
                    .last()
                    .ok_or_else(|| CompileError::CodegenError {
                        message: "break outside loop or switch".into(),
                    })?
                    .break_label;
                self.emit_goto(label);
                Ok(())
            }
            CStmt::Continue => {
                // Search backwards for the first loop context
                let label = self
                    .breakable_stack
                    .iter()
                    .rev()
                    .find(|ctx| ctx.is_loop)
                    .and_then(|ctx| ctx.continue_label)
                    .ok_or_else(|| CompileError::CodegenError {
                        message: "continue outside loop".into(),
                    })?;
                self.emit_goto(label);
                Ok(())
            }
            CStmt::Switch { expr, cases, default_body } => {
                self.gen_switch(expr, cases, default_body.as_deref())?;
                Ok(())
            }
            CStmt::TryCatch { try_body, catches, finally_body } => {
                self.gen_try_catch(try_body, catches, finally_body.as_deref())?;
                Ok(())
            }
        }
    }

    // --- Expression codegen ---

    fn gen_expr(&mut self, expr: &CExpr) -> Result<(), CompileError> {
        match expr {
            CExpr::IntLiteral(v) => {
                self.emit_int_const(*v);
                Ok(())
            }
            CExpr::LongLiteral(v) => {
                self.emit_long_const(*v);
                Ok(())
            }
            CExpr::FloatLiteral(v) => {
                self.emit_float_const(*v as f32);
                Ok(())
            }
            CExpr::DoubleLiteral(v) => {
                self.emit_double_const(*v);
                Ok(())
            }
            CExpr::BoolLiteral(b) => {
                self.emit(if *b {
                    Instruction::Iconst1
                } else {
                    Instruction::Iconst0
                });
                Ok(())
            }
            CExpr::StringLiteral(s) => {
                let cp_idx = self.class_file.get_or_add_string(s);
                self.emit_ldc(cp_idx);
                Ok(())
            }
            CExpr::CharLiteral(c) => {
                self.emit_int_const(*c as i64);
                Ok(())
            }
            CExpr::NullLiteral => {
                self.emit(Instruction::Aconstnull);
                Ok(())
            }
            CExpr::Ident(name) => {
                let (slot, ty) = self.locals.find(name).ok_or_else(|| {
                    CompileError::CodegenError {
                        message: format!("undefined variable: {}", name),
                    }
                })?;
                let ty = ty.clone();
                self.emit_load(&ty, slot);
                Ok(())
            }
            CExpr::This => {
                if self.is_static {
                    return Err(CompileError::CodegenError {
                        message: "'this' not available in static method".into(),
                    });
                }
                self.emit(Instruction::Aload0);
                Ok(())
            }
            CExpr::BinaryOp { op, left, right } => {
                self.gen_expr(left)?;
                self.gen_expr(right)?;
                let instr = match op {
                    BinOp::Add => Instruction::Iadd,
                    BinOp::Sub => Instruction::Isub,
                    BinOp::Mul => Instruction::Imul,
                    BinOp::Div => Instruction::Idiv,
                    BinOp::Rem => Instruction::Irem,
                    BinOp::Shl => Instruction::Ishl,
                    BinOp::Shr => Instruction::Ishr,
                    BinOp::Ushr => Instruction::Iushr,
                    BinOp::BitAnd => Instruction::Iand,
                    BinOp::BitOr => Instruction::Ior,
                    BinOp::BitXor => Instruction::Ixor,
                };
                self.emit(instr);
                Ok(())
            }
            CExpr::UnaryOp { op, operand } => {
                self.gen_expr(operand)?;
                match op {
                    UnaryOp::Neg => {
                        self.emit(Instruction::Ineg);
                    }
                    UnaryOp::BitNot => {
                        // ~x == x ^ -1
                        self.emit(Instruction::Iconstm1);
                        self.emit(Instruction::Ixor);
                    }
                }
                Ok(())
            }
            CExpr::Comparison { op, left, right } => {
                // Evaluate comparison to 0/1 using branches
                self.gen_expr(left)?;
                self.gen_expr(right)?;
                let true_label = self.new_label();
                let end_label = self.new_label();
                let branch = match op {
                    CompareOp::Eq => Instruction::IfIcmpeq as fn(i16) -> Instruction,
                    CompareOp::Ne => Instruction::IfIcmpne,
                    CompareOp::Lt => Instruction::IfIcmplt,
                    CompareOp::Le => Instruction::IfIcmple,
                    CompareOp::Gt => Instruction::IfIcmpgt,
                    CompareOp::Ge => Instruction::IfIcmpge,
                };
                self.emit_branch(branch, true_label);
                self.emit(Instruction::Iconst0);
                self.emit_goto(end_label);
                self.bind_label(true_label);
                self.emit(Instruction::Iconst1);
                self.bind_label(end_label);
                Ok(())
            }
            CExpr::LogicalAnd(left, right) => {
                let false_label = self.new_label();
                let end_label = self.new_label();
                self.gen_condition(left, false_label, false)?;
                self.gen_condition(right, false_label, false)?;
                self.emit(Instruction::Iconst1);
                self.emit_goto(end_label);
                self.bind_label(false_label);
                self.emit(Instruction::Iconst0);
                self.bind_label(end_label);
                Ok(())
            }
            CExpr::LogicalOr(left, right) => {
                let true_label = self.new_label();
                let false_label = self.new_label();
                let end_label = self.new_label();
                self.gen_condition(left, true_label, true)?;
                self.gen_condition(right, false_label, false)?;
                self.bind_label(true_label);
                self.emit(Instruction::Iconst1);
                self.emit_goto(end_label);
                self.bind_label(false_label);
                self.emit(Instruction::Iconst0);
                self.bind_label(end_label);
                Ok(())
            }
            CExpr::LogicalNot(operand) => {
                let true_label = self.new_label();
                let end_label = self.new_label();
                self.gen_condition(operand, true_label, true)?;
                // Condition was false, so !cond is true
                self.emit(Instruction::Iconst1);
                self.emit_goto(end_label);
                self.bind_label(true_label);
                // Condition was true, so !cond is false
                self.emit(Instruction::Iconst0);
                self.bind_label(end_label);
                Ok(())
            }
            CExpr::Assign { target, value } => {
                self.gen_expr(value)?;
                self.emit(Instruction::Dup); // leave value on stack for assignment expression
                self.gen_store_target(target)?;
                Ok(())
            }
            CExpr::CompoundAssign { op, target, value } => {
                // Load current, compute, dup, store
                self.gen_expr(target)?;
                self.gen_expr(value)?;
                let instr = match op {
                    BinOp::Add => Instruction::Iadd,
                    BinOp::Sub => Instruction::Isub,
                    BinOp::Mul => Instruction::Imul,
                    BinOp::Div => Instruction::Idiv,
                    BinOp::Rem => Instruction::Irem,
                    BinOp::Shl => Instruction::Ishl,
                    BinOp::Shr => Instruction::Ishr,
                    BinOp::Ushr => Instruction::Iushr,
                    BinOp::BitAnd => Instruction::Iand,
                    BinOp::BitOr => Instruction::Ior,
                    BinOp::BitXor => Instruction::Ixor,
                };
                self.emit(instr);
                self.emit(Instruction::Dup);
                self.gen_store_target(target)?;
                Ok(())
            }
            CExpr::PreIncrement(operand) => {
                if let CExpr::Ident(name) = operand.as_ref() {
                    let (slot, ty) = self.locals.find(name).ok_or_else(|| {
                        CompileError::CodegenError {
                            message: format!("undefined variable: {}", name),
                        }
                    })?;
                    let ty = ty.clone();
                    let slot = slot;
                    if is_int_type(&ty) && slot <= 255 {
                        self.emit(Instruction::Iinc {
                            index: slot as u8,
                            value: 1,
                        });
                        self.emit_load(&ty, slot);
                    } else {
                        self.emit_load(&ty, slot);
                        self.emit(Instruction::Iconst1);
                        self.emit(Instruction::Iadd);
                        self.emit(Instruction::Dup);
                        self.emit_store(&ty, slot);
                    }
                    Ok(())
                } else {
                    Err(CompileError::CodegenError {
                        message: "pre-increment requires simple variable".into(),
                    })
                }
            }
            CExpr::PreDecrement(operand) => {
                if let CExpr::Ident(name) = operand.as_ref() {
                    let (slot, ty) = self.locals.find(name).ok_or_else(|| {
                        CompileError::CodegenError {
                            message: format!("undefined variable: {}", name),
                        }
                    })?;
                    let ty = ty.clone();
                    let slot = slot;
                    if is_int_type(&ty) && slot <= 255 {
                        self.emit(Instruction::Iinc {
                            index: slot as u8,
                            value: -1,
                        });
                        self.emit_load(&ty, slot);
                    } else {
                        self.emit_load(&ty, slot);
                        self.emit(Instruction::Iconst1);
                        self.emit(Instruction::Isub);
                        self.emit(Instruction::Dup);
                        self.emit_store(&ty, slot);
                    }
                    Ok(())
                } else {
                    Err(CompileError::CodegenError {
                        message: "pre-decrement requires simple variable".into(),
                    })
                }
            }
            CExpr::PostIncrement(operand) => {
                if let CExpr::Ident(name) = operand.as_ref() {
                    let (slot, ty) = self.locals.find(name).ok_or_else(|| {
                        CompileError::CodegenError {
                            message: format!("undefined variable: {}", name),
                        }
                    })?;
                    let ty = ty.clone();
                    let slot = slot;
                    self.emit_load(&ty, slot);
                    if is_int_type(&ty) && slot <= 255 {
                        self.emit(Instruction::Iinc {
                            index: slot as u8,
                            value: 1,
                        });
                    } else {
                        self.emit(Instruction::Dup);
                        self.emit(Instruction::Iconst1);
                        self.emit(Instruction::Iadd);
                        self.emit_store(&ty, slot);
                    }
                    Ok(())
                } else {
                    Err(CompileError::CodegenError {
                        message: "post-increment requires simple variable".into(),
                    })
                }
            }
            CExpr::PostDecrement(operand) => {
                if let CExpr::Ident(name) = operand.as_ref() {
                    let (slot, ty) = self.locals.find(name).ok_or_else(|| {
                        CompileError::CodegenError {
                            message: format!("undefined variable: {}", name),
                        }
                    })?;
                    let ty = ty.clone();
                    let slot = slot;
                    self.emit_load(&ty, slot);
                    if is_int_type(&ty) && slot <= 255 {
                        self.emit(Instruction::Iinc {
                            index: slot as u8,
                            value: -1,
                        });
                    } else {
                        self.emit(Instruction::Dup);
                        self.emit(Instruction::Iconst1);
                        self.emit(Instruction::Isub);
                        self.emit_store(&ty, slot);
                    }
                    Ok(())
                } else {
                    Err(CompileError::CodegenError {
                        message: "post-decrement requires simple variable".into(),
                    })
                }
            }
            CExpr::MethodCall { object, name, args } => {
                self.gen_method_call(object.as_deref(), name, args)
            }
            CExpr::StaticMethodCall {
                class_name,
                name,
                args,
            } => self.gen_static_method_call(class_name, name, args),
            CExpr::FieldAccess { object, name } => self.gen_field_access(object, name),
            CExpr::StaticFieldAccess { class_name, name } => {
                self.gen_static_field_access(class_name, name)
            }
            CExpr::NewObject { class_name, args } => {
                let internal = resolve_class_name(class_name);
                let class_idx = self.class_file.get_or_add_class(&internal);
                self.emit(Instruction::New(class_idx));
                self.emit(Instruction::Dup);
                for arg in args {
                    self.gen_expr(arg)?;
                }
                // Default constructor descriptor — try to infer from arg count
                let descriptor = self.infer_constructor_descriptor(args)?;
                let method_idx =
                    self.class_file
                        .get_or_add_method_ref(&internal, "<init>", &descriptor);
                self.emit(Instruction::Invokespecial(method_idx));
                Ok(())
            }
            CExpr::NewArray { element_type, size } => {
                self.gen_expr(size)?;
                match element_type {
                    TypeName::Primitive(kind) => {
                        let atype = match kind {
                            PrimitiveKind::Boolean => 4,
                            PrimitiveKind::Char => 5,
                            PrimitiveKind::Float => 6,
                            PrimitiveKind::Double => 7,
                            PrimitiveKind::Byte => 8,
                            PrimitiveKind::Short => 9,
                            PrimitiveKind::Int => 10,
                            PrimitiveKind::Long => 11,
                            PrimitiveKind::Void => {
                                return Err(CompileError::CodegenError {
                                    message: "cannot create array of void".into(),
                                })
                            }
                        };
                        self.emit(Instruction::Newarray(atype));
                    }
                    TypeName::Class(name) => {
                        let internal = resolve_class_name(name);
                        let class_idx = self.class_file.get_or_add_class(&internal);
                        self.emit(Instruction::Anewarray(class_idx));
                    }
                    TypeName::Array(_) => {
                        // Multi-dimensional: create array of arrays
                        let descriptor = type_name_to_descriptor(element_type);
                        let class_idx =
                            self.class_file.get_or_add_class(&descriptor);
                        self.emit(Instruction::Anewarray(class_idx));
                    }
                }
                Ok(())
            }
            CExpr::ArrayAccess { array, index } => {
                self.gen_expr(array)?;
                self.gen_expr(index)?;
                // Default to aaload for reference arrays, iaload for int arrays
                self.emit(Instruction::Aaload);
                Ok(())
            }
            CExpr::Cast { ty, operand } => {
                self.gen_expr(operand)?;
                match ty {
                    TypeName::Primitive(kind) => {
                        match kind {
                            PrimitiveKind::Long => self.emit(Instruction::I2l),
                            PrimitiveKind::Float => self.emit(Instruction::I2f),
                            PrimitiveKind::Double => self.emit(Instruction::I2d),
                            PrimitiveKind::Byte => self.emit(Instruction::I2b),
                            PrimitiveKind::Char => self.emit(Instruction::I2c),
                            PrimitiveKind::Short => self.emit(Instruction::I2s),
                            PrimitiveKind::Int => return Ok(()), // no-op for int cast
                            PrimitiveKind::Boolean => return Ok(()),
                            PrimitiveKind::Void => {
                                return Err(CompileError::CodegenError {
                                    message: "cannot cast to void".into(),
                                })
                            }
                        };
                        Ok(())
                    }
                    TypeName::Class(name) => {
                        let internal = resolve_class_name(name);
                        let class_idx = self.class_file.get_or_add_class(&internal);
                        self.emit(Instruction::Checkcast(class_idx));
                        Ok(())
                    }
                    TypeName::Array(_) => {
                        let descriptor = type_name_to_descriptor(ty);
                        let class_idx = self.class_file.get_or_add_class(&descriptor);
                        self.emit(Instruction::Checkcast(class_idx));
                        Ok(())
                    }
                }
            }
            CExpr::Instanceof { operand, ty } => {
                self.gen_expr(operand)?;
                match ty {
                    TypeName::Class(name) => {
                        let internal = resolve_class_name(name);
                        let class_idx = self.class_file.get_or_add_class(&internal);
                        self.emit(Instruction::Instanceof(class_idx));
                    }
                    TypeName::Array(_) => {
                        let descriptor = type_name_to_descriptor(ty);
                        let class_idx = self.class_file.get_or_add_class(&descriptor);
                        self.emit(Instruction::Instanceof(class_idx));
                    }
                    _ => {
                        return Err(CompileError::CodegenError {
                            message: "instanceof requires class or array type".into(),
                        })
                    }
                }
                Ok(())
            }
            CExpr::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                let false_label = self.new_label();
                let end_label = self.new_label();
                self.gen_condition(condition, false_label, false)?;
                self.gen_expr(then_expr)?;
                self.emit_goto(end_label);
                self.bind_label(false_label);
                self.gen_expr(else_expr)?;
                self.bind_label(end_label);
                Ok(())
            }
        }
    }

    // --- Condition codegen (emit direct branch instructions) ---

    /// Generate condition code. If `jump_on_true`, jumps to `target_label` when condition is true.
    /// Otherwise, jumps to `target_label` when condition is false.
    fn gen_condition(
        &mut self,
        expr: &CExpr,
        target_label: usize,
        jump_on_true: bool,
    ) -> Result<(), CompileError> {
        match expr {
            CExpr::Comparison { op, left, right } => {
                // Check for null comparisons
                if matches!(right.as_ref(), CExpr::NullLiteral) {
                    self.gen_expr(left)?;
                    let branch = if jump_on_true {
                        match op {
                            CompareOp::Eq => Instruction::Ifnull as fn(i16) -> Instruction,
                            CompareOp::Ne => Instruction::Ifnonnull,
                            _ => {
                                return Err(CompileError::CodegenError {
                                    message: "cannot compare null with relational operator".into(),
                                })
                            }
                        }
                    } else {
                        match op {
                            CompareOp::Eq => Instruction::Ifnonnull as fn(i16) -> Instruction,
                            CompareOp::Ne => Instruction::Ifnull,
                            _ => {
                                return Err(CompileError::CodegenError {
                                    message: "cannot compare null with relational operator".into(),
                                })
                            }
                        }
                    };
                    self.emit_branch(branch, target_label);
                    return Ok(());
                }
                if matches!(left.as_ref(), CExpr::NullLiteral) {
                    self.gen_expr(right)?;
                    let branch = if jump_on_true {
                        match op {
                            CompareOp::Eq => Instruction::Ifnull as fn(i16) -> Instruction,
                            CompareOp::Ne => Instruction::Ifnonnull,
                            _ => {
                                return Err(CompileError::CodegenError {
                                    message: "cannot compare null with relational operator".into(),
                                })
                            }
                        }
                    } else {
                        match op {
                            CompareOp::Eq => Instruction::Ifnonnull as fn(i16) -> Instruction,
                            CompareOp::Ne => Instruction::Ifnull,
                            _ => {
                                return Err(CompileError::CodegenError {
                                    message: "cannot compare null with relational operator".into(),
                                })
                            }
                        }
                    };
                    self.emit_branch(branch, target_label);
                    return Ok(());
                }

                self.gen_expr(left)?;
                self.gen_expr(right)?;
                let branch = if jump_on_true {
                    match op {
                        CompareOp::Eq => Instruction::IfIcmpeq as fn(i16) -> Instruction,
                        CompareOp::Ne => Instruction::IfIcmpne,
                        CompareOp::Lt => Instruction::IfIcmplt,
                        CompareOp::Le => Instruction::IfIcmple,
                        CompareOp::Gt => Instruction::IfIcmpgt,
                        CompareOp::Ge => Instruction::IfIcmpge,
                    }
                } else {
                    // Inverted branch for "jump on false"
                    match op {
                        CompareOp::Eq => Instruction::IfIcmpne as fn(i16) -> Instruction,
                        CompareOp::Ne => Instruction::IfIcmpeq,
                        CompareOp::Lt => Instruction::IfIcmpge,
                        CompareOp::Le => Instruction::IfIcmpgt,
                        CompareOp::Gt => Instruction::IfIcmple,
                        CompareOp::Ge => Instruction::IfIcmplt,
                    }
                };
                self.emit_branch(branch, target_label);
                Ok(())
            }
            CExpr::LogicalAnd(left, right) => {
                if jump_on_true {
                    // a && b is true: both must be true
                    let skip = self.new_label();
                    self.gen_condition(left, skip, false)?;
                    self.gen_condition(right, target_label, true)?;
                    self.bind_label(skip);
                } else {
                    // a && b is false: either is false
                    self.gen_condition(left, target_label, false)?;
                    self.gen_condition(right, target_label, false)?;
                }
                Ok(())
            }
            CExpr::LogicalOr(left, right) => {
                if jump_on_true {
                    // a || b is true: either is true
                    self.gen_condition(left, target_label, true)?;
                    self.gen_condition(right, target_label, true)?;
                } else {
                    // a || b is false: both must be false
                    let skip = self.new_label();
                    self.gen_condition(left, skip, true)?;
                    self.gen_condition(right, target_label, false)?;
                    self.bind_label(skip);
                }
                Ok(())
            }
            CExpr::LogicalNot(operand) => {
                self.gen_condition(operand, target_label, !jump_on_true)
            }
            CExpr::BoolLiteral(true) => {
                if jump_on_true {
                    self.emit_goto(target_label);
                }
                Ok(())
            }
            CExpr::BoolLiteral(false) => {
                if !jump_on_true {
                    self.emit_goto(target_label);
                }
                Ok(())
            }
            _ => {
                // Generic: evaluate to int, branch on 0/non-0
                self.gen_expr(expr)?;
                let branch = if jump_on_true {
                    Instruction::Ifne as fn(i16) -> Instruction
                } else {
                    Instruction::Ifeq as fn(i16) -> Instruction
                };
                self.emit_branch(branch, target_label);
                Ok(())
            }
        }
    }

    // --- Switch codegen ---

    fn gen_switch(
        &mut self,
        expr: &CExpr,
        cases: &[SwitchCase],
        default_body: Option<&[CStmt]>,
    ) -> Result<(), CompileError> {
        self.gen_expr(expr)?;

        let end_label = self.new_label();
        let default_label = self.new_label();

        // Collect all (value, case_index) pairs
        let mut value_to_case: Vec<(i32, usize)> = Vec::new();
        for (case_idx, case) in cases.iter().enumerate() {
            for &v in &case.values {
                value_to_case.push((v as i32, case_idx));
            }
        }
        value_to_case.sort_by_key(|&(v, _)| v);

        // Create labels for each case body
        let case_labels: Vec<usize> = cases.iter().map(|_| self.new_label()).collect();

        // Decide tableswitch vs lookupswitch
        let use_table = if value_to_case.is_empty() {
            false
        } else {
            let low = value_to_case.first().unwrap().0;
            let high = value_to_case.last().unwrap().0;
            let range = (high as i64 - low as i64 + 1) as usize;
            range <= 2 * value_to_case.len()
        };

        if use_table && !value_to_case.is_empty() {
            let low = value_to_case.first().unwrap().0;
            let high = value_to_case.last().unwrap().0;

            // Build offset labels array: for each index in [low..=high], map to case label or default
            let mut offset_labels: Vec<usize> = Vec::new();
            let mut val_idx = 0;
            for v in low..=high {
                if val_idx < value_to_case.len() && value_to_case[val_idx].0 == v {
                    offset_labels.push(case_labels[value_to_case[val_idx].1]);
                    val_idx += 1;
                } else {
                    offset_labels.push(default_label);
                }
            }

            // Emit placeholder tableswitch
            let placeholder = Instruction::Tableswitch {
                default: 0,
                low,
                high,
                offsets: vec![0i32; offset_labels.len()],
            };
            let instr_idx = self.emit(placeholder);
            self.switch_patches.push(SwitchPatch {
                instr_idx,
                kind: SwitchPatchKind::Table {
                    low,
                    high,
                    case_labels: offset_labels,
                    default_label,
                },
            });
        } else {
            // Lookupswitch
            let pair_labels: Vec<(i32, usize)> = value_to_case
                .iter()
                .map(|&(v, case_idx)| (v, case_labels[case_idx]))
                .collect();

            let placeholder = Instruction::Lookupswitch {
                default: 0,
                npairs: pair_labels.len() as u32,
                pairs: pair_labels.iter().map(|&(v, _)| (v, 0i32)).collect(),
            };
            let instr_idx = self.emit(placeholder);
            self.switch_patches.push(SwitchPatch {
                instr_idx,
                kind: SwitchPatchKind::Lookup {
                    pairs: pair_labels,
                    default_label,
                },
            });
        }

        // Push breakable context
        self.breakable_stack.push(BreakableContext {
            break_label: end_label,
            is_loop: false,
            continue_label: None,
        });

        // Emit case bodies
        for (i, case) in cases.iter().enumerate() {
            self.bind_label(case_labels[i]);
            for s in &case.body {
                self.gen_stmt(s)?;
            }
        }

        // Emit default body
        self.bind_label(default_label);
        if let Some(body) = default_body {
            for s in body {
                self.gen_stmt(s)?;
            }
        }

        self.bind_label(end_label);
        self.breakable_stack.pop();
        Ok(())
    }

    // --- Try-catch codegen ---

    fn gen_try_catch(
        &mut self,
        try_body: &[CStmt],
        catches: &[CatchClause],
        finally_body: Option<&[CStmt]>,
    ) -> Result<(), CompileError> {
        let try_start = self.new_label();
        let try_end = self.new_label();
        let after_all = self.new_label();

        // Capture locals at try-start for exception handler frames and merge point
        let locals_at_try_start = self.locals.current_locals_vtypes();
        // The after_all merge point must use try-start locals, since the try-body exit path
        // doesn't have catch-allocated locals.
        self.label_locals_override
            .push((after_all, locals_at_try_start.clone()));

        // Emit try body
        self.bind_label(try_start);
        for s in try_body {
            self.gen_stmt(s)?;
        }
        self.bind_label(try_end);

        // Inline finally at end of try (if present), then goto after_all
        if let Some(fin_body) = finally_body {
            for s in fin_body {
                self.gen_stmt(s)?;
            }
        }
        self.emit_goto(after_all);

        // Emit each catch handler
        let mut catch_handler_labels = Vec::new();
        for catch in catches {
            let handler_label = self.new_label();
            catch_handler_labels.push(handler_label);
            // Register as exception handler for frame tracking
            let internal = resolve_class_name(match &catch.exception_type {
                TypeName::Class(name) => name.as_str(),
                _ => "java/lang/Exception",
            });
            let ex_class_idx = self.class_file.get_or_add_class(&internal);
            self.exception_handler_labels
                .push((handler_label, VType::Object(ex_class_idx), locals_at_try_start.clone()));
            self.bind_label(handler_label);

            // astore exception to a local
            let ex_slot = self.locals.allocate(&catch.var_name, &catch.exception_type);
            self.emit_store(&catch.exception_type, ex_slot);

            // Emit catch body
            for s in &catch.body {
                self.gen_stmt(s)?;
            }

            // Inline finally (if present)
            if let Some(fin_body) = finally_body {
                for s in fin_body {
                    self.gen_stmt(s)?;
                }
            }
            self.emit_goto(after_all);
        }

        // If finally present: emit catch-all handler that stores exception, runs finally, rethrows
        let catch_all_label = if finally_body.is_some() {
            let label = self.new_label();
            // Register as exception handler for frame tracking
            let throwable_idx = self.class_file.get_or_add_class("java/lang/Throwable");
            self.exception_handler_labels
                .push((label, VType::Object(throwable_idx), locals_at_try_start.clone()));
            self.bind_label(label);
            let ex_ty = TypeName::Class("java/lang/Throwable".into());
            let ex_slot = self.locals.allocate("__finally_ex", &ex_ty);
            self.emit_store(&ex_ty, ex_slot);
            if let Some(fin_body) = finally_body {
                for s in fin_body {
                    self.gen_stmt(s)?;
                }
            }
            self.emit_load(&ex_ty, ex_slot);
            self.emit(Instruction::Athrow);
            Some(label)
        } else {
            None
        };

        self.bind_label(after_all);

        // Register exception table entries
        for (i, catch) in catches.iter().enumerate() {
            let internal = resolve_class_name(match &catch.exception_type {
                TypeName::Class(name) => name.as_str(),
                _ => "java/lang/Exception",
            });
            let catch_type = self.class_file.get_or_add_class(&internal);
            self.pending_exceptions.push(PendingExceptionEntry {
                start_label: try_start,
                end_label: try_end,
                handler_label: catch_handler_labels[i],
                catch_type,
            });
        }

        // Catch-all for finally
        if let Some(catch_all) = catch_all_label {
            self.pending_exceptions.push(PendingExceptionEntry {
                start_label: try_start,
                end_label: try_end,
                handler_label: catch_all,
                catch_type: 0, // 0 = catch all
            });
        }

        Ok(())
    }

    // --- Method/field resolution ---

    fn gen_method_call(
        &mut self,
        object: Option<&CExpr>,
        name: &str,
        args: &[CExpr],
    ) -> Result<(), CompileError> {
        match object {
            Some(obj) => {
                // Check for string concatenation: "something" + ... results in StringBuilder pattern
                // But first, check if this is a chain like System.out.println
                // Try to resolve as chain: detect Class.field.method pattern
                if let Some((class_name, field_chain, is_static_root)) =
                    self.resolve_dot_chain(obj)
                {
                    if is_static_root {
                        // Static field access chain, e.g. System.out
                        // Look up the field ref to get the field type
                        self.gen_static_chain_method_call(
                            &class_name,
                            &field_chain,
                            name,
                            args,
                        )?;
                        return Ok(());
                    }
                }

                // Regular instance method call
                self.gen_expr(obj)?;
                for arg in args {
                    self.gen_expr(arg)?;
                }
                // Try to find the method ref in the constant pool
                let descriptor = self.find_method_descriptor_in_pool(name, args)?;
                let class_name = self.infer_receiver_class(obj)?;
                let method_idx =
                    self.class_file
                        .get_or_add_method_ref(&class_name, name, &descriptor);
                self.emit(Instruction::Invokevirtual(method_idx));
                Ok(())
            }
            None => {
                // Unqualified method call - call on `this`
                if !self.is_static {
                    self.emit(Instruction::Aload0); // this
                }
                for arg in args {
                    self.gen_expr(arg)?;
                }
                let descriptor = self.find_method_descriptor_in_pool(name, args)?;
                let this_class = self.get_this_class_name()?;
                let method_idx =
                    self.class_file
                        .get_or_add_method_ref(&this_class, name, &descriptor);
                if self.is_static {
                    self.emit(Instruction::Invokestatic(method_idx));
                } else {
                    self.emit(Instruction::Invokevirtual(method_idx));
                }
                Ok(())
            }
        }
    }

    fn gen_static_method_call(
        &mut self,
        class_name: &str,
        name: &str,
        args: &[CExpr],
    ) -> Result<(), CompileError> {
        for arg in args {
            self.gen_expr(arg)?;
        }
        let internal = resolve_class_name(class_name);
        let descriptor = self.find_method_descriptor_in_pool(name, args)?;
        let method_idx = self
            .class_file
            .get_or_add_method_ref(&internal, name, &descriptor);
        self.emit(Instruction::Invokestatic(method_idx));
        Ok(())
    }

    fn gen_field_access(&mut self, object: &CExpr, name: &str) -> Result<(), CompileError> {
        // Check if this is a static field access (e.g., System.out)
        if let CExpr::Ident(class_name) = object {
            if class_name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                // Likely static field access
                return self.gen_static_field_access(class_name, name);
            }
        }
        self.gen_expr(object)?;
        let class_name = self.infer_receiver_class(object)?;
        let descriptor = self.find_field_descriptor_in_pool(&class_name, name)?;
        let field_idx =
            self.class_file
                .get_or_add_field_ref(&class_name, name, &descriptor);
        self.emit(Instruction::Getfield(field_idx));
        Ok(())
    }

    fn gen_static_field_access(
        &mut self,
        class_name: &str,
        name: &str,
    ) -> Result<(), CompileError> {
        let internal = resolve_class_name(class_name);
        let descriptor = self.find_field_descriptor_in_pool(&internal, name)?;
        let field_idx =
            self.class_file
                .get_or_add_field_ref(&internal, name, &descriptor);
        self.emit(Instruction::Getstatic(field_idx));
        Ok(())
    }

    /// Handle chains like System.out.println(x):
    /// System is the class, out is the static field, println is the method
    fn gen_static_chain_method_call(
        &mut self,
        class_name: &str,
        field_chain: &[String],
        method_name: &str,
        args: &[CExpr],
    ) -> Result<(), CompileError> {
        let internal = resolve_class_name(class_name);

        // Start with the static field access
        let first_field = &field_chain[0];
        let field_desc = self.find_field_descriptor_in_pool(&internal, first_field)?;
        let field_idx =
            self.class_file
                .get_or_add_field_ref(&internal, first_field, &field_desc);
        self.emit(Instruction::Getstatic(field_idx));

        // For subsequent fields in the chain, use getfield
        let mut current_type_desc = field_desc;
        for field_name in &field_chain[1..] {
            let field_class = descriptor_to_internal(&current_type_desc)?;
            let fd = self.find_field_descriptor_in_pool(&field_class, field_name)?;
            let fi = self
                .class_file
                .get_or_add_field_ref(&field_class, field_name, &fd);
            self.emit(Instruction::Getfield(fi));
            current_type_desc = fd;
        }

        // Now emit args and invoke the method on the field's type
        for arg in args {
            self.gen_expr(arg)?;
        }

        let receiver_class = descriptor_to_internal(&current_type_desc)?;
        let method_desc = self.find_method_descriptor_in_pool(method_name, args)?;
        let method_idx = self.class_file.get_or_add_method_ref(
            &receiver_class,
            method_name,
            &method_desc,
        );
        self.emit(Instruction::Invokevirtual(method_idx));
        Ok(())
    }

    /// Try to resolve a dot-chain to (root_class, [field_names], is_static).
    fn resolve_dot_chain(&self, expr: &CExpr) -> Option<(String, Vec<String>, bool)> {
        let mut fields = Vec::new();
        let mut current = expr;

        loop {
            match current {
                CExpr::FieldAccess { object, name } => {
                    fields.push(name.clone());
                    current = object;
                }
                CExpr::Ident(name) => {
                    // Check if this is a class name (starts with uppercase)
                    if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                        fields.reverse();
                        return Some((name.clone(), fields, true));
                    }
                    return None;
                }
                _ => return None,
            }
        }
    }

    // --- Store target ---

    fn gen_store_target(&mut self, target: &CExpr) -> Result<(), CompileError> {
        match target {
            CExpr::Ident(name) => {
                let (slot, ty) = self.locals.find(name).ok_or_else(|| {
                    CompileError::CodegenError {
                        message: format!("undefined variable: {}", name),
                    }
                })?;
                let ty = ty.clone();
                self.emit_store(&ty, slot);
                Ok(())
            }
            CExpr::FieldAccess { object, name } => {
                // value is on stack, we need object ref below it
                // This is tricky — for simple cases, re-emit the object load
                // TODO: handle more complex cases
                self.gen_expr(object)?;
                self.emit(Instruction::Swap);
                let class_name = self.infer_receiver_class(object)?;
                let descriptor = self.find_field_descriptor_in_pool(&class_name, name)?;
                let field_idx =
                    self.class_file
                        .get_or_add_field_ref(&class_name, name, &descriptor);
                self.emit(Instruction::Putfield(field_idx));
                Ok(())
            }
            CExpr::ArrayAccess { array, index } => {
                self.gen_expr(array)?;
                self.gen_expr(index)?;
                // Rotate: value, array, index -> array, index, value
                // Use dup_x2 + pop pattern
                // Actually the value was dup'd before calling gen_store_target,
                // so stack is: ..., value(dup'd), and we need to store into array[index]
                // We need: arrayref, index, value
                // Hmm, this needs careful stack manipulation. For MVP, just re-evaluate.
                self.emit(Instruction::Iastore); // or aastore
                Ok(())
            }
            _ => Err(CompileError::CodegenError {
                message: "invalid assignment target".into(),
            }),
        }
    }

    // --- Helper: does this expression leave a value on the stack? ---

    fn expr_leaves_value(&self, expr: &CExpr) -> bool {
        match expr {
            CExpr::Assign { .. } | CExpr::CompoundAssign { .. } => true,
            CExpr::PostIncrement(_) | CExpr::PostDecrement(_) => true,
            CExpr::PreIncrement(_) | CExpr::PreDecrement(_) => true,
            CExpr::MethodCall { object, name, .. } => {
                // Check if the method returns void by looking up in pool
                // For simplicity, assume non-void unless we can determine otherwise
                // Well-known void methods:
                if name == "println" || name == "print" || name == "close" || name == "flush" {
                    // These are commonly void but we still need to check
                    // For now, check if the call is on a PrintStream-like object
                    if let Some(obj) = object {
                        if self.is_print_stream_chain(obj) {
                            return false;
                        }
                    }
                }
                // Try to find method descriptor to check return type
                if let Ok(desc) = self.find_method_descriptor_in_pool(name, &[]) {
                    if desc.ends_with(")V") || desc.ends_with("V") {
                        return false;
                    }
                }
                true
            }
            CExpr::StaticMethodCall { name, .. } => {
                if let Ok(desc) = self.find_method_descriptor_in_pool(name, &[]) {
                    if desc.ends_with(")V") || desc.ends_with("V") {
                        return false;
                    }
                }
                true
            }
            _ => true,
        }
    }

    fn is_print_stream_chain(&self, expr: &CExpr) -> bool {
        // Detect System.out pattern
        if let CExpr::FieldAccess { object, name } = expr {
            if name == "out" || name == "err" {
                if let CExpr::Ident(class_name) = object.as_ref() {
                    if class_name == "System" {
                        return true;
                    }
                }
            }
        }
        false
    }

    // --- Instruction emission helpers ---

    fn emit_int_const(&mut self, value: i64) {
        match value {
            -1 => { self.emit(Instruction::Iconstm1); }
            0 => { self.emit(Instruction::Iconst0); }
            1 => { self.emit(Instruction::Iconst1); }
            2 => { self.emit(Instruction::Iconst2); }
            3 => { self.emit(Instruction::Iconst3); }
            4 => { self.emit(Instruction::Iconst4); }
            5 => { self.emit(Instruction::Iconst5); }
            v if v >= -128 && v <= 127 => { self.emit(Instruction::Bipush(v as i8)); }
            v if v >= -32768 && v <= 32767 => { self.emit(Instruction::Sipush(v as i16)); }
            v => {
                let cp_idx = self.class_file.get_or_add_integer(v as i32);
                self.emit_ldc(cp_idx);
            }
        }
    }

    fn emit_long_const(&mut self, value: i64) {
        match value {
            0 => { self.emit(Instruction::Lconst0); }
            1 => { self.emit(Instruction::Lconst1); }
            _ => {
                let cp_idx = self.class_file.get_or_add_long(value);
                self.emit(Instruction::Ldc2W(cp_idx));
            }
        }
    }

    fn emit_float_const(&mut self, value: f32) {
        if value == 0.0 && value.is_sign_positive() {
            self.emit(Instruction::Fconst0);
        } else if value == 1.0 {
            self.emit(Instruction::Fconst1);
        } else if value == 2.0 {
            self.emit(Instruction::Fconst2);
        } else {
            let cp_idx = self.class_file.get_or_add_float(value);
            self.emit_ldc(cp_idx);
            return;
        };
    }

    fn emit_double_const(&mut self, value: f64) {
        if value == 0.0 && value.is_sign_positive() {
            self.emit(Instruction::Dconst0);
        } else if value == 1.0 {
            self.emit(Instruction::Dconst1);
        } else {
            let cp_idx = self.class_file.get_or_add_double(value);
            self.emit(Instruction::Ldc2W(cp_idx));
            return;
        };
    }

    fn emit_ldc(&mut self, cp_idx: u16) {
        if cp_idx <= 255 {
            self.emit(Instruction::Ldc(cp_idx as u8));
        } else {
            self.emit(Instruction::LdcW(cp_idx));
        }
    }

    fn emit_load(&mut self, ty: &TypeName, slot: u16) {
        if is_reference_type(ty) {
            match slot {
                0 => self.emit(Instruction::Aload0),
                1 => self.emit(Instruction::Aload1),
                2 => self.emit(Instruction::Aload2),
                3 => self.emit(Instruction::Aload3),
                s if s <= 255 => self.emit(Instruction::Aload(s as u8)),
                s => self.emit(Instruction::AloadWide(s)),
            };
        } else if is_long_type(ty) {
            match slot {
                0 => self.emit(Instruction::Lload0),
                1 => self.emit(Instruction::Lload1),
                2 => self.emit(Instruction::Lload2),
                3 => self.emit(Instruction::Lload3),
                s if s <= 255 => self.emit(Instruction::Lload(s as u8)),
                s => self.emit(Instruction::LloadWide(s)),
            };
        } else if is_float_type(ty) {
            match slot {
                0 => self.emit(Instruction::Fload0),
                1 => self.emit(Instruction::Fload1),
                2 => self.emit(Instruction::Fload2),
                3 => self.emit(Instruction::Fload3),
                s if s <= 255 => self.emit(Instruction::Fload(s as u8)),
                s => self.emit(Instruction::FloadWide(s)),
            };
        } else if is_double_type(ty) {
            match slot {
                0 => self.emit(Instruction::Dload0),
                1 => self.emit(Instruction::Dload1),
                2 => self.emit(Instruction::Dload2),
                3 => self.emit(Instruction::Dload3),
                s if s <= 255 => self.emit(Instruction::Dload(s as u8)),
                s => self.emit(Instruction::DloadWide(s)),
            };
        } else {
            // int and friends
            match slot {
                0 => self.emit(Instruction::Iload0),
                1 => self.emit(Instruction::Iload1),
                2 => self.emit(Instruction::Iload2),
                3 => self.emit(Instruction::Iload3),
                s if s <= 255 => self.emit(Instruction::Iload(s as u8)),
                s => self.emit(Instruction::IloadWide(s)),
            };
        }
    }

    fn emit_store(&mut self, ty: &TypeName, slot: u16) {
        if is_reference_type(ty) {
            match slot {
                0 => self.emit(Instruction::Astore0),
                1 => self.emit(Instruction::Astore1),
                2 => self.emit(Instruction::Astore2),
                3 => self.emit(Instruction::Astore3),
                s if s <= 255 => self.emit(Instruction::Astore(s as u8)),
                s => self.emit(Instruction::AstoreWide(s)),
            };
        } else if is_long_type(ty) {
            match slot {
                0 => self.emit(Instruction::Lstore0),
                1 => self.emit(Instruction::Lstore1),
                2 => self.emit(Instruction::Lstore2),
                3 => self.emit(Instruction::Lstore3),
                s if s <= 255 => self.emit(Instruction::Lstore(s as u8)),
                s => self.emit(Instruction::LstoreWide(s)),
            };
        } else if is_float_type(ty) {
            match slot {
                0 => self.emit(Instruction::Fstore0),
                1 => self.emit(Instruction::Fstore1),
                2 => self.emit(Instruction::Fstore2),
                3 => self.emit(Instruction::Fstore3),
                s if s <= 255 => self.emit(Instruction::Fstore(s as u8)),
                s => self.emit(Instruction::FstoreWide(s)),
            };
        } else if is_double_type(ty) {
            match slot {
                0 => self.emit(Instruction::Dstore0),
                1 => self.emit(Instruction::Dstore1),
                2 => self.emit(Instruction::Dstore2),
                3 => self.emit(Instruction::Dstore3),
                s if s <= 255 => self.emit(Instruction::Dstore(s as u8)),
                s => self.emit(Instruction::DstoreWide(s)),
            };
        } else {
            match slot {
                0 => self.emit(Instruction::Istore0),
                1 => self.emit(Instruction::Istore1),
                2 => self.emit(Instruction::Istore2),
                3 => self.emit(Instruction::Istore3),
                s if s <= 255 => self.emit(Instruction::Istore(s as u8)),
                s => self.emit(Instruction::IstoreWide(s)),
            };
        }
    }

    // --- Type/descriptor resolution helpers ---

    fn get_this_class_name(&self) -> Result<String, CompileError> {
        use crate::constant_info::ConstantInfo;
        let this_class = self.class_file.this_class;
        match &self.class_file.const_pool[(this_class - 1) as usize] {
            ConstantInfo::Class(c) => {
                self.class_file.get_utf8(c.name_index).map(|s| s.to_string()).ok_or_else(|| {
                    CompileError::CodegenError {
                        message: "could not resolve this class name".into(),
                    }
                })
            }
            _ => Err(CompileError::CodegenError {
                message: "this_class does not point to a Class constant".into(),
            }),
        }
    }

    fn infer_receiver_class(&self, expr: &CExpr) -> Result<String, CompileError> {
        match expr {
            CExpr::This => self.get_this_class_name(),
            CExpr::Ident(name) => {
                // Check local variable type
                if let Some((_, ty)) = self.locals.find(name) {
                    if let TypeName::Class(class_name) = ty {
                        return Ok(resolve_class_name(class_name));
                    }
                }
                // Might be a class name for static access
                if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                    return Ok(resolve_class_name(name));
                }
                Err(CompileError::CodegenError {
                    message: format!("cannot infer class for receiver '{}'", name),
                })
            }
            CExpr::FieldAccess { object, name: field_name } => {
                // Try to find the field's type in the constant pool
                if let Ok(class_name) = self.infer_receiver_class(object) {
                    if let Ok(desc) = self.find_field_descriptor_in_pool(&class_name, field_name) {
                        if let Ok(internal) = descriptor_to_internal(&desc) {
                            return Ok(internal);
                        }
                    }
                }
                Err(CompileError::CodegenError {
                    message: format!("cannot infer class for field access '{}'", field_name),
                })
            }
            CExpr::MethodCall { name, .. } => {
                // Try to infer from method return type in pool
                Err(CompileError::CodegenError {
                    message: format!("cannot infer class for method call '{}'", name),
                })
            }
            CExpr::StringLiteral(_) => Ok("java/lang/String".into()),
            CExpr::NewObject { class_name, .. } => Ok(resolve_class_name(class_name)),
            _ => Err(CompileError::CodegenError {
                message: "cannot infer receiver class".into(),
            }),
        }
    }

    /// Resolve method descriptor. For well-known overloaded methods, infers from
    /// argument types. For others, searches the constant pool.
    fn find_method_descriptor_in_pool(
        &self,
        method_name: &str,
        args: &[CExpr],
    ) -> Result<String, CompileError> {
        // Well-known overloaded methods — infer from arg types first to avoid
        // picking the wrong overload from the pool
        match method_name {
            "println" => return match args.len() {
                0 => Ok("()V".into()),
                1 => Ok(self.infer_println_descriptor(&args[0])),
                _ => Ok("(Ljava/lang/String;)V".into()),
            },
            "print" => return match args.len() {
                1 => Ok(self.infer_println_descriptor(&args[0])),
                _ => Ok("(Ljava/lang/String;)V".into()),
            },
            "append" if args.len() == 1 => return Ok(self.infer_append_descriptor(&args[0])),
            "toString" if args.is_empty() => return Ok("()Ljava/lang/String;".into()),
            "equals" if args.len() == 1 => return Ok("(Ljava/lang/Object;)Z".into()),
            "hashCode" if args.is_empty() => return Ok("()I".into()),
            "length" if args.is_empty() => return Ok("()I".into()),
            "charAt" if args.len() == 1 => return Ok("(I)C".into()),
            "substring" if args.len() == 1 => return Ok("(I)Ljava/lang/String;".into()),
            "substring" if args.len() == 2 => return Ok("(II)Ljava/lang/String;".into()),
            "valueOf" if args.len() == 1 => return Ok("(Ljava/lang/Object;)Ljava/lang/String;".into()),
            "parseInt" if args.len() == 1 => return Ok("(Ljava/lang/String;)I".into()),
            "getClass" if args.is_empty() => return Ok("()Ljava/lang/Class;".into()),
            _ => {}
        }

        // Search constant pool for matching method
        use crate::constant_info::ConstantInfo;
        let pool = &self.class_file.const_pool;

        for entry in pool.iter() {
            let nat_index = match entry {
                ConstantInfo::MethodRef(r) => r.name_and_type_index,
                ConstantInfo::InterfaceMethodRef(r) => r.name_and_type_index,
                _ => continue,
            };
            if let ConstantInfo::NameAndType(nat) = &pool[(nat_index - 1) as usize] {
                if let Some(name) = self.class_file.get_utf8(nat.name_index) {
                    if name == method_name {
                        if let Some(desc) = self.class_file.get_utf8(nat.descriptor_index) {
                            if let Some((params, _)) = parse_method_descriptor(desc) {
                                if params.len() == args.len() {
                                    return Ok(desc.to_string());
                                }
                            }
                        }
                    }
                }
            }
        }

        Err(CompileError::CodegenError {
            message: format!(
                "cannot find method '{}' with {} args in constant pool; \
                 consider using a class with this method already referenced",
                method_name,
                args.len()
            ),
        })
    }

    fn infer_println_descriptor(&self, arg: &CExpr) -> String {
        match arg {
            CExpr::StringLiteral(_) => "(Ljava/lang/String;)V".into(),
            CExpr::IntLiteral(_) => "(I)V".into(),
            CExpr::LongLiteral(_) => "(J)V".into(),
            CExpr::FloatLiteral(_) => "(F)V".into(),
            CExpr::DoubleLiteral(_) => "(D)V".into(),
            CExpr::BoolLiteral(_) => "(Z)V".into(),
            CExpr::CharLiteral(_) => "(C)V".into(),
            CExpr::Ident(name) => {
                if let Some((_, ty)) = self.locals.find(name) {
                    match ty {
                        TypeName::Primitive(PrimitiveKind::Int) => return "(I)V".into(),
                        TypeName::Primitive(PrimitiveKind::Long) => return "(J)V".into(),
                        TypeName::Primitive(PrimitiveKind::Float) => return "(F)V".into(),
                        TypeName::Primitive(PrimitiveKind::Double) => return "(D)V".into(),
                        TypeName::Primitive(PrimitiveKind::Boolean) => return "(Z)V".into(),
                        TypeName::Primitive(PrimitiveKind::Char) => return "(C)V".into(),
                        TypeName::Class(name) if name == "String" || name == "java.lang.String" => {
                            return "(Ljava/lang/String;)V".into()
                        }
                        _ => {}
                    }
                }
                "(Ljava/lang/Object;)V".into()
            }
            // For method calls and other complex expressions, assume Object
            _ => "(Ljava/lang/Object;)V".into(),
        }
    }

    fn infer_append_descriptor(&self, arg: &CExpr) -> String {
        match arg {
            CExpr::StringLiteral(_) => {
                "(Ljava/lang/String;)Ljava/lang/StringBuilder;".into()
            }
            CExpr::IntLiteral(_) => "(I)Ljava/lang/StringBuilder;".into(),
            CExpr::CharLiteral(_) => "(C)Ljava/lang/StringBuilder;".into(),
            CExpr::Ident(name) => {
                if let Some((_, ty)) = self.locals.find(name) {
                    match ty {
                        TypeName::Primitive(PrimitiveKind::Int) => {
                            return "(I)Ljava/lang/StringBuilder;".into()
                        }
                        TypeName::Primitive(PrimitiveKind::Long) => {
                            return "(J)Ljava/lang/StringBuilder;".into()
                        }
                        TypeName::Primitive(PrimitiveKind::Float) => {
                            return "(F)Ljava/lang/StringBuilder;".into()
                        }
                        TypeName::Primitive(PrimitiveKind::Double) => {
                            return "(D)Ljava/lang/StringBuilder;".into()
                        }
                        TypeName::Primitive(PrimitiveKind::Char) => {
                            return "(C)Ljava/lang/StringBuilder;".into()
                        }
                        TypeName::Class(name)
                            if name == "String" || name == "java.lang.String" =>
                        {
                            return "(Ljava/lang/String;)Ljava/lang/StringBuilder;".into()
                        }
                        _ => {}
                    }
                }
                "(Ljava/lang/Object;)Ljava/lang/StringBuilder;".into()
            }
            _ => "(Ljava/lang/Object;)Ljava/lang/StringBuilder;".into(),
        }
    }

    fn infer_constructor_descriptor(
        &self,
        args: &[CExpr],
    ) -> Result<String, CompileError> {
        if args.is_empty() {
            return Ok("()V".into());
        }
        // Build descriptor from arg types
        let mut desc = String::from("(");
        for arg in args {
            desc.push_str(&self.infer_arg_descriptor(arg));
        }
        desc.push_str(")V");
        Ok(desc)
    }

    fn infer_arg_descriptor(&self, arg: &CExpr) -> String {
        match arg {
            CExpr::StringLiteral(_) => "Ljava/lang/String;".into(),
            CExpr::IntLiteral(_) => "I".into(),
            CExpr::LongLiteral(_) => "J".into(),
            CExpr::FloatLiteral(_) => "F".into(),
            CExpr::DoubleLiteral(_) => "D".into(),
            CExpr::BoolLiteral(_) => "Z".into(),
            CExpr::CharLiteral(_) => "C".into(),
            CExpr::Ident(name) => {
                if let Some((_, ty)) = self.locals.find(name) {
                    return type_name_to_descriptor(ty);
                }
                "Ljava/lang/Object;".into()
            }
            _ => "Ljava/lang/Object;".into(),
        }
    }

    fn find_field_descriptor_in_pool(
        &self,
        class_name: &str,
        field_name: &str,
    ) -> Result<String, CompileError> {
        use crate::constant_info::ConstantInfo;
        let pool = &self.class_file.const_pool;

        for entry in pool.iter() {
            if let ConstantInfo::FieldRef(r) = entry {
                // Check class
                if let ConstantInfo::Class(c) = &pool[(r.class_index - 1) as usize] {
                    if let Some(cn) = self.class_file.get_utf8(c.name_index) {
                        if cn == class_name {
                            if let ConstantInfo::NameAndType(nat) =
                                &pool[(r.name_and_type_index - 1) as usize]
                            {
                                if let Some(name) = self.class_file.get_utf8(nat.name_index) {
                                    if name == field_name {
                                        if let Some(desc) =
                                            self.class_file.get_utf8(nat.descriptor_index)
                                        {
                                            return Ok(desc.to_string());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Well-known fields
        match (class_name, field_name) {
            ("java/lang/System", "out") => Ok("Ljava/io/PrintStream;".into()),
            ("java/lang/System", "err") => Ok("Ljava/io/PrintStream;".into()),
            ("java/lang/System", "in") => Ok("Ljava/io/InputStream;".into()),
            ("java/lang/Boolean", "TRUE") => Ok("Ljava/lang/Boolean;".into()),
            ("java/lang/Boolean", "FALSE") => Ok("Ljava/lang/Boolean;".into()),
            _ => Err(CompileError::CodegenError {
                message: format!(
                    "cannot find field '{}.{}' in constant pool",
                    class_name, field_name
                ),
            }),
        }
    }
}

// --- Utility functions ---

fn resolve_label_addr(
    label_id: usize,
    labels: &[Option<usize>],
    addresses: &[u32],
    end_addr: i32,
) -> Result<i32, CompileError> {
    let target_instr = labels[label_id].ok_or_else(|| CompileError::CodegenError {
        message: format!("unresolved label {}", label_id),
    })?;
    if target_instr < addresses.len() {
        Ok(addresses[target_instr] as i32)
    } else {
        Ok(end_addr)
    }
}

/// Compute the byte offset at a specific instruction index.
fn compute_byte_offset_at(instructions: &[Instruction], target_idx: usize) -> u32 {
    let mut addr = 0u32;
    for (i, instr) in instructions.iter().enumerate() {
        if i == target_idx {
            return addr;
        }
        addr += instruction_byte_size(instr, addr);
    }
    addr // past-end offset
}

fn compute_byte_addresses(instructions: &[Instruction]) -> Vec<u32> {
    let mut addresses = Vec::with_capacity(instructions.len());
    let mut addr = 0u32;
    for instr in instructions {
        addresses.push(addr);
        addr += instruction_byte_size(instr, addr);
    }
    addresses
}

fn patch_branch_offset(instr: &Instruction, offset: i16) -> Result<Instruction, CompileError> {
    Ok(match instr {
        Instruction::Goto(_) => Instruction::Goto(offset),
        Instruction::Ifeq(_) => Instruction::Ifeq(offset),
        Instruction::Ifne(_) => Instruction::Ifne(offset),
        Instruction::Iflt(_) => Instruction::Iflt(offset),
        Instruction::Ifge(_) => Instruction::Ifge(offset),
        Instruction::Ifgt(_) => Instruction::Ifgt(offset),
        Instruction::Ifle(_) => Instruction::Ifle(offset),
        Instruction::IfIcmpeq(_) => Instruction::IfIcmpeq(offset),
        Instruction::IfIcmpne(_) => Instruction::IfIcmpne(offset),
        Instruction::IfIcmplt(_) => Instruction::IfIcmplt(offset),
        Instruction::IfIcmpge(_) => Instruction::IfIcmpge(offset),
        Instruction::IfIcmpgt(_) => Instruction::IfIcmpgt(offset),
        Instruction::IfIcmple(_) => Instruction::IfIcmple(offset),
        Instruction::IfAcmpeq(_) => Instruction::IfAcmpeq(offset),
        Instruction::IfAcmpne(_) => Instruction::IfAcmpne(offset),
        Instruction::Ifnull(_) => Instruction::Ifnull(offset),
        Instruction::Ifnonnull(_) => Instruction::Ifnonnull(offset),
        _ => {
            return Err(CompileError::CodegenError {
                message: format!("cannot patch branch offset on {:?}", instr),
            })
        }
    })
}

/// Resolve a simple or dotted class name to JVM internal form.
pub fn resolve_class_name(name: &str) -> String {
    // Well-known short names
    match name {
        "String" => return "java/lang/String".into(),
        "Object" => return "java/lang/Object".into(),
        "System" => return "java/lang/System".into(),
        "Integer" => return "java/lang/Integer".into(),
        "Long" => return "java/lang/Long".into(),
        "Float" => return "java/lang/Float".into(),
        "Double" => return "java/lang/Double".into(),
        "Boolean" => return "java/lang/Boolean".into(),
        "Byte" => return "java/lang/Byte".into(),
        "Character" => return "java/lang/Character".into(),
        "Short" => return "java/lang/Short".into(),
        "Math" => return "java/lang/Math".into(),
        "StringBuilder" => return "java/lang/StringBuilder".into(),
        "StringBuffer" => return "java/lang/StringBuffer".into(),
        "PrintStream" => return "java/io/PrintStream".into(),
        "InputStream" => return "java/io/InputStream".into(),
        "Exception" => return "java/lang/Exception".into(),
        "RuntimeException" => return "java/lang/RuntimeException".into(),
        "NullPointerException" => return "java/lang/NullPointerException".into(),
        "IllegalArgumentException" => return "java/lang/IllegalArgumentException".into(),
        "IllegalStateException" => return "java/lang/IllegalStateException".into(),
        "UnsupportedOperationException" => {
            return "java/lang/UnsupportedOperationException".into()
        }
        "IndexOutOfBoundsException" => return "java/lang/IndexOutOfBoundsException".into(),
        "ArrayList" => return "java/util/ArrayList".into(),
        "HashMap" => return "java/util/HashMap".into(),
        "List" => return "java/util/List".into(),
        "Map" => return "java/util/Map".into(),
        "Set" => return "java/util/Set".into(),
        "Arrays" => return "java/util/Arrays".into(),
        "Collections" => return "java/util/Collections".into(),
        "Class" => return "java/lang/Class".into(),
        _ => {}
    }
    // Convert dotted name to internal form
    name.replace('.', "/")
}

fn type_name_to_descriptor(ty: &TypeName) -> String {
    match ty {
        TypeName::Primitive(kind) => match kind {
            PrimitiveKind::Int => "I".into(),
            PrimitiveKind::Long => "J".into(),
            PrimitiveKind::Float => "F".into(),
            PrimitiveKind::Double => "D".into(),
            PrimitiveKind::Boolean => "Z".into(),
            PrimitiveKind::Byte => "B".into(),
            PrimitiveKind::Char => "C".into(),
            PrimitiveKind::Short => "S".into(),
            PrimitiveKind::Void => "V".into(),
        },
        TypeName::Class(name) => {
            let internal = resolve_class_name(name);
            format!("L{};", internal)
        }
        TypeName::Array(inner) => {
            format!("[{}", type_name_to_descriptor(inner))
        }
    }
}

fn descriptor_to_internal(desc: &str) -> Result<String, CompileError> {
    if desc.starts_with('L') && desc.ends_with(';') {
        Ok(desc[1..desc.len() - 1].to_string())
    } else {
        Err(CompileError::CodegenError {
            message: format!("cannot convert descriptor '{}' to internal class name", desc),
        })
    }
}

fn jvm_type_to_type_name(jvm_ty: &JvmType) -> TypeName {
    match jvm_ty {
        JvmType::Int => TypeName::Primitive(PrimitiveKind::Int),
        JvmType::Long => TypeName::Primitive(PrimitiveKind::Long),
        JvmType::Float => TypeName::Primitive(PrimitiveKind::Float),
        JvmType::Double => TypeName::Primitive(PrimitiveKind::Double),
        JvmType::Boolean => TypeName::Primitive(PrimitiveKind::Boolean),
        JvmType::Byte => TypeName::Primitive(PrimitiveKind::Byte),
        JvmType::Char => TypeName::Primitive(PrimitiveKind::Char),
        JvmType::Short => TypeName::Primitive(PrimitiveKind::Short),
        JvmType::Void => TypeName::Primitive(PrimitiveKind::Void),
        JvmType::Reference(name) => TypeName::Class(name.clone()),
        JvmType::Array(inner) => TypeName::Array(Box::new(jvm_type_to_type_name(inner))),
        JvmType::Null | JvmType::Unknown => TypeName::Class("java/lang/Object".into()),
    }
}

fn is_int_type(ty: &TypeName) -> bool {
    matches!(
        ty,
        TypeName::Primitive(
            PrimitiveKind::Int
                | PrimitiveKind::Boolean
                | PrimitiveKind::Byte
                | PrimitiveKind::Char
                | PrimitiveKind::Short
        )
    )
}

fn is_long_type(ty: &TypeName) -> bool {
    matches!(ty, TypeName::Primitive(PrimitiveKind::Long))
}

fn is_float_type(ty: &TypeName) -> bool {
    matches!(ty, TypeName::Primitive(PrimitiveKind::Float))
}

fn is_double_type(ty: &TypeName) -> bool {
    matches!(ty, TypeName::Primitive(PrimitiveKind::Double))
}

fn is_reference_type(ty: &TypeName) -> bool {
    matches!(ty, TypeName::Class(_) | TypeName::Array(_))
}

fn type_slot_width(ty: &TypeName) -> u16 {
    match ty {
        TypeName::Primitive(PrimitiveKind::Long | PrimitiveKind::Double) => 2,
        _ => 1,
    }
}

fn type_name_to_vtype(ty: &TypeName) -> VType {
    match ty {
        TypeName::Primitive(kind) => match kind {
            PrimitiveKind::Int | PrimitiveKind::Boolean | PrimitiveKind::Byte
            | PrimitiveKind::Char | PrimitiveKind::Short => VType::Integer,
            PrimitiveKind::Long => VType::Long,
            PrimitiveKind::Float => VType::Float,
            PrimitiveKind::Double => VType::Double,
            PrimitiveKind::Void => VType::Top,
        },
        TypeName::Class(_) | TypeName::Array(_) => {
            // We'd need a class_file reference to resolve the cp index.
            // Use a sentinel — this will be resolved later or use Null as fallback.
            VType::Null
        }
    }
}

fn jvm_type_to_vtype(jvm_ty: &JvmType) -> VType {
    match jvm_ty {
        JvmType::Int | JvmType::Boolean | JvmType::Byte | JvmType::Char | JvmType::Short => {
            VType::Integer
        }
        JvmType::Long => VType::Long,
        JvmType::Float => VType::Float,
        JvmType::Double => VType::Double,
        JvmType::Void => VType::Top,
        JvmType::Reference(_) | JvmType::Array(_) | JvmType::Null | JvmType::Unknown => {
            VType::Null
        }
    }
}
