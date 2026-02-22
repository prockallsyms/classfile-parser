use crate::attribute_info::{AttributeInfo, AttributeInfoVariant, CodeAttribute};
use crate::constant_info::ConstantInfo;
use crate::method_info::MethodAccessFlags;
use crate::ClassFile;

use super::codegen::CodeGenerator;
use super::lexer::Lexer;
use super::parser::Parser;
use super::{CompileError, CompileOptions};

/// Compile Java source and replace a method's body in the class file.
pub fn compile_method_body_impl(
    source: &str,
    class_file: &mut ClassFile,
    method_name: &str,
    options: &CompileOptions,
) -> Result<(), CompileError> {
    // Find the method
    let method_idx = class_file
        .methods
        .iter()
        .position(|m| {
            matches!(
                &class_file.const_pool[(m.name_index - 1) as usize],
                ConstantInfo::Utf8(u) if u.utf8_string == method_name
            )
        })
        .ok_or_else(|| CompileError::MethodNotFound {
            name: method_name.to_string(),
        })?;

    // Get method info
    let is_static = class_file.methods[method_idx]
        .access_flags
        .contains(MethodAccessFlags::STATIC);
    let descriptor_index = class_file.methods[method_idx].descriptor_index;
    let method_descriptor = class_file
        .get_utf8(descriptor_index)
        .ok_or_else(|| CompileError::CodegenError {
            message: "could not resolve method descriptor".into(),
        })?
        .to_string();

    // Parse source
    let lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse_method_body()?;

    // Generate bytecode
    let mut codegen = CodeGenerator::new_with_options(
        class_file,
        is_static,
        &method_descriptor,
        options.generate_stack_map_table,
    )?;
    codegen.generate_body(&stmts)?;
    let generated = codegen.finish()?;

    // Build StackMapTable sub-attribute if generated
    let smt_sub_attr = if options.generate_stack_map_table {
        if let Some(smt) = generated.stack_map_table {
            let smt_name_idx = class_file.get_or_add_utf8("StackMapTable");
            let mut smt_attr = AttributeInfo {
                attribute_name_index: smt_name_idx,
                attribute_length: 0,
                info: vec![],
                info_parsed: Some(AttributeInfoVariant::StackMapTable(smt)),
            };
            smt_attr.sync_from_parsed().map_err(|e| CompileError::CodegenError {
                message: format!("sync_from_parsed for StackMapTable failed: {}", e),
            })?;
            Some(smt_attr)
        } else {
            None
        }
    } else {
        None
    };

    // Find or create Code attribute
    let code_attr_idx = class_file.methods[method_idx]
        .attributes
        .iter()
        .position(|a| matches!(a.info_parsed, Some(AttributeInfoVariant::Code(_))));

    if let Some(attr_idx) = code_attr_idx {
        let code = match &mut class_file.methods[method_idx].attributes[attr_idx].info_parsed {
            Some(AttributeInfoVariant::Code(c)) => c,
            _ => unreachable!(),
        };

        // Replace instructions and update stack/locals
        code.code = generated.instructions;
        code.max_stack = generated.max_stack;
        code.max_locals = generated.max_locals;
        code.exception_table = generated.exception_table;
        code.exception_table_length = code.exception_table.len() as u16;

        // Strip debug and verification sub-attributes that reference old bytecode offsets
        code.attributes.retain(|a| {
            !matches!(
                a.info_parsed,
                Some(AttributeInfoVariant::LineNumberTable(_))
                    | Some(AttributeInfoVariant::LocalVariableTable(_))
                    | Some(AttributeInfoVariant::LocalVariableTypeTable(_))
            )
        });
        // Always strip old StackMapTable
        code.attributes.retain(|a| {
            !matches!(
                a.info_parsed,
                Some(AttributeInfoVariant::StackMapTable(_))
            )
        });
        // Attach new StackMapTable if generated
        if let Some(smt_attr) = smt_sub_attr {
            code.attributes.push(smt_attr);
        }
        code.attributes_count = code.attributes.len() as u16;

        // Sync
        class_file.methods[method_idx].attributes[attr_idx]
            .sync_from_parsed()
            .map_err(|e| CompileError::CodegenError {
                message: format!("sync_from_parsed failed: {}", e),
            })?;
    } else {
        // Create a new Code attribute
        let code_name_idx = class_file.get_or_add_utf8("Code");
        let exception_table_length = generated.exception_table.len() as u16;

        let mut sub_attrs = Vec::new();
        if let Some(smt_attr) = smt_sub_attr {
            sub_attrs.push(smt_attr);
        }

        let code_attr = CodeAttribute {
            max_stack: generated.max_stack,
            max_locals: generated.max_locals,
            code_length: 0, // will be set by sync
            code: generated.instructions,
            exception_table_length,
            exception_table: generated.exception_table,
            attributes_count: sub_attrs.len() as u16,
            attributes: sub_attrs,
        };

        let mut attr_info = AttributeInfo {
            attribute_name_index: code_name_idx,
            attribute_length: 0,
            info: vec![],
            info_parsed: Some(AttributeInfoVariant::Code(code_attr)),
        };
        attr_info.sync_from_parsed().map_err(|e| CompileError::CodegenError {
            message: format!("sync_from_parsed failed: {}", e),
        })?;

        class_file.methods[method_idx].attributes.push(attr_info);
        class_file.methods[method_idx].attributes_count =
            class_file.methods[method_idx].attributes.len() as u16;
    }

    class_file.sync_counts();
    Ok(())
}
