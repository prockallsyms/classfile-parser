pub mod lexer;
pub mod ast;
pub mod parser;
pub mod codegen;
pub mod stack_calc;
pub mod stackmap;
pub mod patch;

use std::fmt;

use crate::attribute_info::{ExceptionEntry, StackMapTableAttribute};
use crate::code_attribute::Instruction;
use crate::ClassFile;

use self::ast::CStmt;
use self::codegen::CodeGenerator;
use self::lexer::Lexer;
use self::parser::Parser;

#[derive(Clone, Debug)]
pub enum CompileError {
    ParseError {
        line: usize,
        column: usize,
        message: String,
    },
    TypeError {
        message: String,
    },
    CodegenError {
        message: String,
    },
    MethodNotFound {
        name: String,
    },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::ParseError {
                line,
                column,
                message,
            } => write!(f, "parse error at {}:{}: {}", line, column, message),
            CompileError::TypeError { message } => write!(f, "type error: {}", message),
            CompileError::CodegenError { message } => write!(f, "codegen error: {}", message),
            CompileError::MethodNotFound { name } => write!(f, "method not found: {}", name),
        }
    }
}

pub struct CompileOptions {
    pub strip_stack_map_table: bool,
    pub generate_stack_map_table: bool,
}

impl Default for CompileOptions {
    fn default() -> Self {
        CompileOptions {
            strip_stack_map_table: true,
            generate_stack_map_table: false,
        }
    }
}

pub struct GeneratedCode {
    pub instructions: Vec<Instruction>,
    pub max_stack: u16,
    pub max_locals: u16,
    pub exception_table: Vec<ExceptionEntry>,
    pub stack_map_table: Option<StackMapTableAttribute>,
}

/// Parse a Java method body into AST statements.
pub fn parse_method_body(source: &str) -> Result<Vec<CStmt>, CompileError> {
    let lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    parser.parse_method_body()
}

/// Generate bytecode from AST statements.
pub fn generate_bytecode(
    stmts: &[CStmt],
    class_file: &mut ClassFile,
    is_static: bool,
    method_descriptor: &str,
) -> Result<GeneratedCode, CompileError> {
    generate_bytecode_with_options(stmts, class_file, is_static, method_descriptor, false)
}

/// Generate bytecode from AST statements with options.
pub fn generate_bytecode_with_options(
    stmts: &[CStmt],
    class_file: &mut ClassFile,
    is_static: bool,
    method_descriptor: &str,
    generate_stack_map_table: bool,
) -> Result<GeneratedCode, CompileError> {
    let mut codegen =
        CodeGenerator::new_with_options(class_file, is_static, method_descriptor, generate_stack_map_table)?;
    codegen.generate_body(stmts)?;
    codegen.finish()
}

/// Compile Java source and replace a method's body in the class file.
pub fn compile_method_body(
    source: &str,
    class_file: &mut ClassFile,
    method_name: &str,
    options: &CompileOptions,
) -> Result<(), CompileError> {
    patch::compile_method_body_impl(source, class_file, method_name, options)
}

/// Compile and patch a single method body in a class file.
///
/// Generates a valid StackMapTable by default so the patched class passes
/// full JVM bytecode verification.
///
/// # Forms
///
/// ```ignore
/// // With StackMapTable generation (passes full verification):
/// patch_method!(class_file, "main", r#"{ System.out.println("hello"); }"#)?;
///
/// // Without StackMapTable (requires -noverify or -XX:-BytecodeVerification*):
/// patch_method!(class_file, "main", r#"{ System.out.println("hello"); }"#, no_verify)?;
/// ```
#[macro_export]
macro_rules! patch_method {
    ($class_file:expr, $method:expr, $source:expr) => {
        $crate::compile::compile_method_body(
            $source,
            &mut $class_file,
            $method,
            &$crate::compile::CompileOptions {
                generate_stack_map_table: true,
                ..$crate::compile::CompileOptions::default()
            },
        )
    };
    ($class_file:expr, $method:expr, $source:expr, no_verify) => {
        $crate::compile::compile_method_body(
            $source,
            &mut $class_file,
            $method,
            &$crate::compile::CompileOptions::default(),
        )
    };
}

/// Compile and patch multiple method bodies in a class file.
///
/// Each method is compiled and patched in order. If any method fails,
/// the error is returned immediately and subsequent methods are not patched.
///
/// Generates a valid StackMapTable by default.
///
/// ```ignore
/// patch_methods!(class_file, {
///     "main"   => r#"{ System.out.println("hello"); }"#,
///     "helper" => r#"{ return 42; }"#,
/// })?;
///
/// // Without StackMapTable:
/// patch_methods!(class_file, no_verify, {
///     "main" => r#"{ System.out.println("hello"); }"#,
/// })?;
/// ```
#[macro_export]
macro_rules! patch_methods {
    ($class_file:expr, { $($method:expr => $source:expr),+ $(,)? }) => {{
        (|| -> Result<(), $crate::compile::CompileError> {
            $(
                $crate::patch_method!($class_file, $method, $source)?;
            )+
            Ok(())
        })()
    }};
    ($class_file:expr, no_verify, { $($method:expr => $source:expr),+ $(,)? }) => {{
        (|| -> Result<(), $crate::compile::CompileError> {
            $(
                $crate::patch_method!($class_file, $method, $source, no_verify)?;
            )+
            Ok(())
        })()
    }};
}
