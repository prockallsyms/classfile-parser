# Java Classfile Parser

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.txt)
![Rust](https://github.com/Palmr/classfile-parser/workflows/Rust/badge.svg)
[![Crates.io Version](https://img.shields.io/crates/v/classfile-parser.svg)](https://crates.io/crates/classfile-parser)

A parser for [Java Classfiles](https://docs.oracle.com/javase/specs/jvms/se10/html/jvms-4.html), written in Rust using [binrw](https://github.com/jam1garner/binrw).

Supports reading, modifying, and writing class files with full round-trip fidelity. Optional features provide JAR archive handling and Spring Boot fat JAR support.

## Installation

Classfile Parser is available from crates.io and can be included in your Cargo enabled project like this:

```toml
[dependencies]
classfile-parser = "~0.3"
```

### Optional features

```toml
# Bytecode-to-Java source decompiler
classfile-parser = { version = "~0.3", features = ["decompile"] }

# JAR archive reading/writing
classfile-parser = { version = "~0.3", features = ["jar-utils"] }

# Spring Boot fat JAR support (includes jar-utils)
classfile-parser = { version = "~0.3", features = ["spring-utils"] }

# Java-to-bytecode compiler for patching method bodies (includes decompile)
classfile-parser = { version = "~0.3", features = ["compile"] }
```

## Usage

### Parsing a class file

```rust
use classfile_parser::ClassFile;
use binrw::BinRead;
use std::io::Cursor;

fn main() {
    let classfile_bytes = include_bytes!("../path/to/JavaClass.class");
    let class_file = ClassFile::read(&mut Cursor::new(classfile_bytes))
        .expect("Failed to parse class file");

    println!(
        "version {},{} const_pool({}), this=const[{}], super=const[{}], \
         interfaces({}), fields({}), methods({}), attributes({}), access({:?})",
        class_file.major_version,
        class_file.minor_version,
        class_file.const_pool_size,
        class_file.this_class,
        class_file.super_class,
        class_file.interfaces_count,
        class_file.fields_count,
        class_file.methods_count,
        class_file.attributes_count,
        class_file.access_flags
    );

    // Look up names via the constant pool
    if let Some(name) = class_file.get_utf8(class_file.this_class) {
        println!("Class name: {name}");
    }
}
```

### Modifying and writing a class file

```rust
use classfile_parser::ClassFile;
use classfile_parser::code_attribute::Instruction;
use binrw::{BinRead, BinWrite};
use std::io::Cursor;

fn main() {
    let classfile_bytes = include_bytes!("../path/to/JavaClass.class");
    let mut class_file = ClassFile::read(&mut Cursor::new(classfile_bytes))
        .expect("Failed to parse class file");

    // Find a method and modify its bytecode
    if let Some(method) = class_file.find_method_mut("main") {
        method.with_code(|code| {
            // Replace the first instruction with a nop
            code.replace_instruction(0, Instruction::Nop);
        });
    }

    // Sync counts and write back out
    class_file.sync_all().expect("sync failed");
    let mut output = Cursor::new(Vec::new());
    class_file.write(&mut output).expect("write failed");
}
```

### Working with JAR files

Requires the `jar-utils` feature. An example script for this feature can be run with `cargo run --example jar_explorer --features tui-example -- path/to/your/jar/file.jar`.

```rust
use classfile_parser::jar_utils::JarFile;

fn main() {
    let jar = JarFile::open("path/to/file.jar").expect("Failed to open JAR");

    for name in jar.entry_names() {
        println!("{name}");
    }

    // Parse a class directly from the JAR
    let class_file = jar.parse_class("com/example/Main.class")
        .expect("Failed to parse class");

    // Read and modify the manifest
    if let Ok(Some(manifest)) = jar.manifest() {
        if let Some(main_class) = manifest.main_attr("Main-Class") {
            println!("Main-Class: {main_class}");
        }
    }
}
```

### Decompiling class files

Requires the `decompile` feature. Converts parsed bytecode back to readable Java source.

```rust
use classfile_parser::ClassFile;
use classfile_parser::decompile::{self, Decompiler, DecompileOptions, RenderConfig};
use binrw::BinRead;
use std::io::Cursor;

fn main() {
    let bytes = include_bytes!("../path/to/JavaClass.class");
    let class = ClassFile::read(&mut Cursor::new(bytes))
        .expect("Failed to parse class file");

    // Quick one-liner with default options
    let source = decompile::decompile(&class).expect("decompilation failed");
    println!("{source}");

    // Or configure the decompiler
    let options = DecompileOptions {
        render_config: RenderConfig {
            indent: "  ".into(),
            max_line_width: 100,
            use_var: false,
            include_synthetic: false,
        },
        include_synthetic: false,
        ..Default::default()
    };
    let decompiler = Decompiler::new(options);
    let source = decompiler.decompile(&class).expect("decompilation failed");
    println!("{source}");

    // Decompile a single method
    let method_src = decompiler.decompile_method(&class, "main")
        .expect("method decompilation failed");
    println!("{method_src}");
}
```

### Compiling and patching method bodies

Requires the `compile` feature. Replaces method bodies in an existing class file with new Java source compiled directly to bytecode — no `javac` needed at runtime.

The `patch_method!` macro is the easiest way to patch a method. It compiles the given Java method body, generates a valid StackMapTable, and replaces the named method in one call:

```rust
use classfile_parser::{patch_method, ClassFile};

fn main() {
    let bytes = std::fs::read("HelloWorld.class").unwrap();
    let mut class_file = ClassFile::from_bytes(&bytes).unwrap();

    patch_method!(class_file, "main", r#"{
        System.out.println("patched!");
    }"#).unwrap();

    std::fs::write("HelloWorld.class", class_file.to_bytes().unwrap()).unwrap();
}
```

Use `patch_methods!` to patch several methods at once:

```rust
use classfile_parser::{patch_methods, ClassFile};

fn main() {
    let bytes = std::fs::read("MyClass.class").unwrap();
    let mut cf = ClassFile::from_bytes(&bytes).unwrap();

    patch_methods!(cf, {
        "main" => r#"{
            int x = 42;
            if (x > 10) {
                System.out.println("big");
            } else {
                System.out.println("small");
            }
        }"#,
        "helper" => r#"{ return 99; }"#,
    }).unwrap();

    std::fs::write("MyClass.class", cf.to_bytes().unwrap()).unwrap();
}
```

Both macros generate a StackMapTable by default, so patched classes pass full JVM bytecode verification. Pass `no_verify` to skip generation if you'll run with `-noverify`:

```rust
patch_method!(class_file, "main", r#"{ return; }"#, no_verify).unwrap();
```

The compiler supports: local variables, arithmetic, if/else, while, for, break/continue, switch (tableswitch/lookupswitch), try-catch-finally, return, throw, method calls, field access, object creation, arrays, casts, instanceof, and ternary expressions.

A full working example is at [`examples/compile_patch.rs`](examples/compile_patch.rs):

```sh
cargo run --example compile_patch --features compile
```

### Spring Boot fat JARs

Requires the `spring-utils` feature.

```rust
use classfile_parser::spring_utils::SpringBootJar;

fn main() {
    if let Ok(Some(sb)) = SpringBootJar::open("path/to/app.jar") {
        println!("Format: {:?}", sb.format());
        println!("Start-Class: {:?}", sb.start_class());

        for name in sb.app_class_names() {
            println!("  {name}");
        }

        for name in sb.nested_jar_names() {
            println!("  lib: {name}");
        }
    }
}
```

## Implementation Status

- [x] Header
  - [x] Magic const
  - [x] Version info
- [x] Constant pool
  - [x] Constant pool size
  - [x] Constant types
    - [x] Utf8
    - [x] Integer
    - [x] Float
    - [x] Long
    - [x] Double
    - [x] Class
    - [x] String
    - [x] Fieldref
    - [x] Methodref
    - [x] InterfaceMethodref
    - [x] NameAndType
    - [x] MethodHandle
    - [x] MethodType
    - [x] InvokeDynamic
    - [x] Module
    - [x] Package
- [x] Access flags
- [x] This class
- [x] Super class
- [x] Interfaces
- [x] Fields
- [x] Methods
- [x] Attributes
  - [x] Basic attribute info block parsing
  - [x] Known typed attributes parsing
    - [x] Critical for JVM
      - [x] ConstantValue
      - [x] Code
      - [x] StackMapTable
      - [x] Exceptions
      - [x] BootstrapMethods
    - [x] Critical for Java SE
      - [x] InnerClasses
      - [x] EnclosingMethod
      - [x] Synthetic
      - [x] Signature
      - [x] RuntimeVisibleAnnotations
      - [x] RuntimeInvisibleAnnotations
      - [x] RuntimeVisibleParameterAnnotations
      - [x] RuntimeInvisibleParameterAnnotations
      - [x] RuntimeVisibleTypeAnnotations
      - [x] RuntimeInvisibleTypeAnnotations
      - [x] AnnotationDefault
      - [x] MethodParameters
    - [x] Useful but not critical
      - [x] SourceFile
      - [~] SourceDebugExtension
      - [x] LineNumberTable
      - [x] LocalVariableTable
      - [x] LocalVariableTypeTable
      - [x] Deprecated
    - [x] Java 9+ module system
      - [x] Module
      - [x] ModulePackages
      - [x] ModuleMainClass
    - [x] Java 11+ nesting
      - [x] NestHost
      - [x] NestMembers
    - [x] Java 16+ records and sealed classes
      - [x] Record
      - [x] PermittedSubclasses
- [x] Instructions
  - [x] All 200+ JVM opcodes
  - [x] Wide instruction variants
  - [x] tableswitch / lookupswitch with alignment padding
- [x] Read-write round-trip support (BinRead + BinWrite)
- [x] Patching support
  - [x] `sync_from_parsed()` for attribute reserialization
  - [x] `sync_counts()` / `sync_lengths()` for count field recalculation
  - [x] `sync_all()` for full class file resync
  - [x] Instruction replacement and nop-out helpers
  - [x] Constant pool addition helpers (`add_utf8`, `add_string`, `add_class`, etc.)
- [x] JAR utilities (optional `jar-utils` feature)
  - [x] Read/write JAR archives
  - [x] Parse class files directly from JARs
  - [x] Manifest parsing and serialization
- [x] Spring Boot support (optional `spring-utils` feature)
  - [x] Detect JAR/WAR format
  - [x] Application class and resource enumeration
  - [x] Nested JAR access
  - [x] classpath.idx and layers.idx parsing
- [x] Decompiler (optional `decompile` feature)
  - [x] Control flow graph construction from bytecode
  - [x] Stack simulation and expression tree recovery
  - [x] Control flow structuring (if/else, while, for, switch, try-catch)
  - [x] Type inference, generics, and annotation recovery
  - [x] Java source rendering with import management
  - [x] Record, sealed class, and enum support
  - [x] Per-method error recovery with bytecode fallback
  - [x] Inner class decompilation
  - [x] Compiler desugaring (autoboxing, for-each, assert)
- [x] Compiler (optional `compile` feature)
  - [x] Java method body lexer, parser, and AST
  - [x] Bytecode codegen (locals, arithmetic, comparisons, logical ops)
  - [x] Control flow: if/else, while, for, break/continue, switch (tableswitch/lookupswitch)
  - [x] Exception handling: try-catch-finally with exception table generation
  - [x] Object creation, method calls, field access, arrays, casts, instanceof, ternary
  - [x] StackMapTable generation for full JVM bytecode verification
  - [x] Method body patching (`compile_method_body`, `patch_method!`, `patch_methods!`)
