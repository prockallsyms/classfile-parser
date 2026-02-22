#![cfg(feature = "compile")]

use std::fs;
use std::io::{Cursor, Read};
use std::process::Command;

use binrw::prelude::*;
use binrw::BinWrite;
use classfile_parser::compile::{
    compile_method_body, generate_bytecode, parse_method_body, CompileOptions,
};
use classfile_parser::code_attribute::Instruction;
use classfile_parser::ClassFile;

// --- Test helpers ---

fn java_available() -> bool {
    Command::new("javac")
        .arg("-version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
        && Command::new("java")
            .arg("-version")
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
}

#[allow(unused)]
fn compile_and_load(test_name: &str, java_src: &str, class_name: &str) -> (std::path::PathBuf, std::path::PathBuf, ClassFile) {
    let tmp_dir = std::env::temp_dir().join(format!("classfile_compile_{}", test_name));
    let _ = fs::remove_dir_all(&tmp_dir);
    fs::create_dir_all(&tmp_dir).unwrap();

    let compile = Command::new("javac")
        .arg("-d")
        .arg(&tmp_dir)
        .arg(java_src)
        .output()
        .expect("failed to run javac");
    assert!(
        compile.status.success(),
        "javac failed: {}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let class_path = tmp_dir.join(format!("{}.class", class_name));
    let mut class_bytes = Vec::new();
    std::fs::File::open(&class_path)
        .expect("failed to open compiled class")
        .read_to_end(&mut class_bytes)
        .unwrap();
    let class_file = ClassFile::read(&mut Cursor::new(&class_bytes)).expect("failed to parse class");

    (tmp_dir, class_path, class_file)
}

fn write_and_run(
    tmp_dir: &std::path::Path,
    class_path: &std::path::Path,
    class_file: &ClassFile,
    class_name: &str,
) -> String {
    let mut out = Cursor::new(Vec::new());
    class_file.write(&mut out).expect("failed to write class");
    fs::write(class_path, out.into_inner()).expect("failed to write class file");

    let run = Command::new("java")
        .arg("-XX:+UnlockDiagnosticVMOptions")
        .arg("-XX:-BytecodeVerificationRemote")
        .arg("-XX:-BytecodeVerificationLocal")
        .arg("-cp")
        .arg(tmp_dir)
        .arg(class_name)
        .output()
        .expect("failed to run java");
    assert!(
        run.status.success(),
        "java failed (exit {}): stderr={}",
        run.status,
        String::from_utf8_lossy(&run.stderr)
    );
    String::from_utf8_lossy(&run.stdout).trim().to_string()
}

// --- Parser tests ---

#[test]
fn test_parse_return_int() {
    let stmts = parse_method_body("{ return 42; }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_local_decl() {
    let stmts = parse_method_body("{ int x = 10; return x; }").unwrap();
    assert_eq!(stmts.len(), 2);
}

#[test]
fn test_parse_if_else() {
    let stmts = parse_method_body("{ if (x > 0) { return 1; } else { return -1; } }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_while_loop() {
    let stmts = parse_method_body("{ while (i < 10) { i = i + 1; } }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_for_loop() {
    let stmts = parse_method_body("{ for (int i = 0; i < 10; i++) { sum += i; } }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_method_call() {
    let stmts = parse_method_body("{ System.out.println(\"hello\"); }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_string_concat() {
    let stmts = parse_method_body("{ String s = \"hello\" + \" world\"; }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_new_object() {
    let stmts = parse_method_body("{ StringBuilder sb = new StringBuilder(); }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_comparison_ops() {
    let stmts = parse_method_body("{ return a == b && c != d || e < f; }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_ternary() {
    let stmts = parse_method_body("{ return x > 0 ? x : -x; }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_array_access() {
    let stmts = parse_method_body("{ return arr[0]; }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_cast() {
    let stmts = parse_method_body("{ long x = (long) y; }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_throw() {
    let stmts = parse_method_body("{ throw new RuntimeException(\"error\"); }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_break_continue() {
    let stmts = parse_method_body("{ while (true) { if (done) break; continue; } }").unwrap();
    assert_eq!(stmts.len(), 1);
}

#[test]
fn test_parse_compound_assign() {
    let stmts = parse_method_body("{ x += 1; y -= 2; z *= 3; }").unwrap();
    assert_eq!(stmts.len(), 3);
}

#[test]
fn test_parse_increment_decrement() {
    let stmts = parse_method_body("{ i++; --j; }").unwrap();
    assert_eq!(stmts.len(), 2);
}

// --- Codegen unit tests ---

#[test]
fn test_codegen_return_42() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("codegen_ret42", "java-assets/src/HelloWorld.java", "HelloWorld");

    let stmts = parse_method_body("{ return; }").unwrap();
    let generated = generate_bytecode(&stmts, &mut class_file, true, "([Ljava/lang/String;)V").unwrap();

    // Should contain Return instruction
    assert!(generated.instructions.iter().any(|i| matches!(i, Instruction::Return)));
    assert!(generated.max_stack >= 1);
    assert!(generated.max_locals >= 1);
}

// --- E2E tests ---

#[test]
fn test_compile_e2e_hello_compiled() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_hello", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{ System.out.println("Compiled!"); }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "Compiled!", "expected 'Compiled!' but got: {}", output);
}

#[test]
fn test_compile_e2e_return_value() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_retval", "java-assets/src/SimpleMath.java", "SimpleMath");

    // Replace intMath to return a different formula: a constant
    compile_method_body(
        r#"{ System.out.println(99); }"#,
        &mut class_file,
        "intMath",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "SimpleMath");
    assert!(
        output.contains("99"),
        "expected 99 in output: {}",
        output
    );
}

#[test]
fn test_compile_e2e_if_else() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_ifelse", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{
            int x = 10;
            if (x > 5) {
                System.out.println("big");
            } else {
                System.out.println("small");
            }
        }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "big", "expected 'big' but got: {}", output);
}

#[test]
fn test_compile_e2e_while_loop() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_while", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{
            int sum = 0;
            int i = 1;
            while (i <= 10) {
                sum = sum + i;
                i = i + 1;
            }
            System.out.println(sum);
        }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "55", "expected '55' but got: {}", output);
}

#[test]
fn test_compile_e2e_for_loop() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_for", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{
            int sum = 0;
            for (int i = 1; i <= 5; i = i + 1) {
                sum = sum + i;
            }
            System.out.println(sum);
        }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "15", "expected '15' but got: {}", output);
}

#[test]
fn test_compile_e2e_arithmetic() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_arith", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{
            int a = 10;
            int b = 3;
            int c = a * b + a / b - a % b;
            System.out.println(c);
        }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    // 10*3 + 10/3 - 10%3 = 30 + 3 - 1 = 32
    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "32", "expected '32' but got: {}", output);
}

#[test]
fn test_compile_e2e_nested_if() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_nested_if", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{
            int x = 15;
            if (x > 10) {
                if (x > 20) {
                    System.out.println("very big");
                } else {
                    System.out.println("medium");
                }
            } else {
                System.out.println("small");
            }
        }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "medium", "expected 'medium' but got: {}", output);
}

// --- Parser tests for switch and try-catch ---

#[test]
fn test_parse_switch() {
    let stmts = parse_method_body(
        "{ switch (x) { case 1: return 1; case 2: case 3: return 23; default: return 0; } }",
    )
    .unwrap();
    assert_eq!(stmts.len(), 1);
    match &stmts[0] {
        classfile_parser::compile::ast::CStmt::Switch {
            cases,
            default_body,
            ..
        } => {
            assert_eq!(cases.len(), 2);
            assert_eq!(cases[0].values, vec![1]);
            assert_eq!(cases[1].values, vec![2, 3]); // fall-through grouping
            assert!(default_body.is_some());
        }
        other => panic!("expected Switch, got {:?}", other),
    }
}

#[test]
fn test_parse_try_catch() {
    let stmts = parse_method_body(
        "{ try { foo(); } catch (Exception e) { bar(); } finally { baz(); } }",
    )
    .unwrap();
    assert_eq!(stmts.len(), 1);
    match &stmts[0] {
        classfile_parser::compile::ast::CStmt::TryCatch {
            catches,
            finally_body,
            ..
        } => {
            assert_eq!(catches.len(), 1);
            assert_eq!(catches[0].var_name, "e");
            assert!(finally_body.is_some());
        }
        other => panic!("expected TryCatch, got {:?}", other),
    }
}

#[test]
fn test_parse_try_multiple_catches() {
    let stmts = parse_method_body(
        "{ try { foo(); } catch (RuntimeException e) { a(); } catch (Exception e) { b(); } }",
    )
    .unwrap();
    assert_eq!(stmts.len(), 1);
    match &stmts[0] {
        classfile_parser::compile::ast::CStmt::TryCatch { catches, .. } => {
            assert_eq!(catches.len(), 2);
        }
        other => panic!("expected TryCatch, got {:?}", other),
    }
}

// --- E2E tests for switch ---

#[test]
fn test_compile_e2e_switch() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_switch", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{
            int x = 2;
            switch (x) {
                case 1:
                    System.out.println("one");
                    break;
                case 2:
                    System.out.println("two");
                    break;
                case 3:
                    System.out.println("three");
                    break;
                default:
                    System.out.println("other");
            }
        }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "two", "expected 'two' but got: {}", output);
}

#[test]
fn test_compile_e2e_switch_default() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_switch_default", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{
            int x = 99;
            switch (x) {
                case 1:
                    System.out.println("one");
                    break;
                case 2:
                    System.out.println("two");
                    break;
                default:
                    System.out.println("default");
            }
        }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "default", "expected 'default' but got: {}", output);
}

// --- E2E tests for try-catch ---

#[test]
fn test_compile_e2e_try_catch() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_try_catch", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{
            try {
                throw new RuntimeException("boom");
            } catch (RuntimeException e) {
                System.out.println("caught");
            }
        }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "caught", "expected 'caught' but got: {}", output);
}

#[test]
fn test_compile_e2e_try_finally() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_try_finally", "java-assets/src/HelloWorld.java", "HelloWorld");

    compile_method_body(
        r#"{
            try {
                System.out.println("try");
            } finally {
                System.out.println("finally");
            }
        }"#,
        &mut class_file,
        "main",
        &CompileOptions::default(),
    )
    .unwrap();

    let output = write_and_run(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "try\nfinally", "expected 'try\\nfinally' but got: {}", output);
}

// --- StackMapTable generation tests ---

/// Run a class with full bytecode verification (no -XX:-BytecodeVerification flags)
fn write_and_run_verified(
    tmp_dir: &std::path::Path,
    class_path: &std::path::Path,
    class_file: &ClassFile,
    class_name: &str,
) -> String {
    let mut out = Cursor::new(Vec::new());
    class_file.write(&mut out).expect("failed to write class");
    fs::write(class_path, out.into_inner()).expect("failed to write class file");

    let run = Command::new("java")
        .arg("-cp")
        .arg(tmp_dir)
        .arg(class_name)
        .output()
        .expect("failed to run java");
    assert!(
        run.status.success(),
        "java (with verification) failed (exit {}): stderr={}",
        run.status,
        String::from_utf8_lossy(&run.stderr)
    );
    String::from_utf8_lossy(&run.stdout).trim().to_string()
}

#[test]
fn test_compile_e2e_stackmap_if_else() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_smt_ifelse", "java-assets/src/HelloWorld.java", "HelloWorld");

    let opts = CompileOptions {
        strip_stack_map_table: false,
        generate_stack_map_table: true,
    };
    compile_method_body(
        r#"{
            int x = 10;
            if (x > 5) {
                System.out.println("big");
            } else {
                System.out.println("small");
            }
        }"#,
        &mut class_file,
        "main",
        &opts,
    )
    .unwrap();

    let output = write_and_run_verified(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "big", "expected 'big' but got: {}", output);
}

#[test]
fn test_compile_e2e_stackmap_while() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_smt_while", "java-assets/src/HelloWorld.java", "HelloWorld");

    let opts = CompileOptions {
        strip_stack_map_table: false,
        generate_stack_map_table: true,
    };
    compile_method_body(
        r#"{
            int sum = 0;
            int i = 1;
            while (i <= 10) {
                sum = sum + i;
                i = i + 1;
            }
            System.out.println(sum);
        }"#,
        &mut class_file,
        "main",
        &opts,
    )
    .unwrap();

    let output = write_and_run_verified(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "55", "expected '55' but got: {}", output);
}

#[test]
fn test_compile_e2e_stackmap_try_catch() {
    if !java_available() {
        eprintln!("skipping: javac/java not found");
        return;
    }
    let (tmp_dir, class_path, mut class_file) =
        compile_and_load("e2e_smt_trycatch", "java-assets/src/HelloWorld.java", "HelloWorld");

    let opts = CompileOptions {
        strip_stack_map_table: false,
        generate_stack_map_table: true,
    };
    compile_method_body(
        r#"{
            try {
                throw new RuntimeException("boom");
            } catch (RuntimeException e) {
                System.out.println("caught");
            }
        }"#,
        &mut class_file,
        "main",
        &opts,
    )
    .unwrap();

    let output = write_and_run_verified(&tmp_dir, &class_path, &class_file, "HelloWorld");
    assert_eq!(output, "caught", "expected 'caught' but got: {}", output);
}
