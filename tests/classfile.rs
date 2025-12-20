extern crate classfile_parser;
extern crate nom;

use std::io::Cursor;

use classfile_parser::class_parser;
use classfile_parser::constant_info::ConstantInfo;

#[test]
fn test_valid_class() {
    let valid_class = include_bytes!("../java-assets/compiled-classes/BasicClass.class");
    let res = class_parser(valid_class);
    match res {
        Result::Ok((_, c)) => {
            println!(
                "Valid class file, version {},{} const_pool({}), this=const[{}], super=const[{}], interfaces({}), fields({}), methods({}), attributes({}), access({:?})",
                c.major_version,
                c.minor_version,
                c.const_pool_size,
                c.this_class,
                c.super_class,
                c.interfaces_count,
                c.fields_count,
                c.methods_count,
                c.attributes_count,
                c.access_flags
            );

            let mut code_const_index = 0;

            println!("Constant pool:");
            for (const_index, const_item) in c.const_pool.iter().enumerate() {
                println!("\t[{}] = {:?}", (const_index + 1), const_item);
                if let ConstantInfo::Utf8(ref c) = *const_item
                    && c.utf8_string.to_string() == "Code"
                {
                    code_const_index = (const_index + 1) as u16;
                }
            }
            println!("Code index = {}", code_const_index);

            println!("Interfaces:");
            for (interface_index, i) in c.interfaces.iter().enumerate() {
                println!(
                    "\t[{}] = const[{}] = {:?}",
                    interface_index,
                    i,
                    c.const_pool[(i - 1) as usize]
                );
            }
            println!("Fields:");
            for (field_index, f) in c.fields.iter().enumerate() {
                println!(
                    "\t[{}] Name(const[{}] = {:?}) - access({:?})",
                    field_index,
                    f.name_index,
                    c.const_pool[(f.name_index - 1) as usize],
                    f.access_flags
                );
            }
            println!("Methods:");
            for (method_index, m) in c.methods.iter().enumerate() {
                println!(
                    "\t[{}] Name(const[{}] = {:?}) - access({:?})",
                    method_index,
                    m.name_index,
                    c.const_pool[(m.name_index - 1) as usize],
                    m.access_flags
                );

                for a in &m.attributes {
                    if a.attribute_name_index == code_const_index {
                        println!("\t\tCode attr found, len = {}", a.attribute_length);
                        let code_result =
                            classfile_parser::attribute_info::code_attribute_parser(&a.info);
                        match code_result {
                            Result::Ok((_, code)) => {
                                println!("\t\t\tCode! code_length = {}", code.code_length);
                            }
                            _ => panic!("Not a valid code attr?"),
                        }
                    } else {
                        println!("\t\tAttribute: {:?}", a);
                    }
                }
            }
        }
        _ => panic!("Not a class file"),
    };
}

#[test]
fn test_utf_string_constants() {
    use binrw::prelude::*;
    use classfile_parser::ClassFile;
    use std::fs::File;
    use std::io::prelude::*;

    let mut contents: Vec<u8> = Vec::new();
    let mut valid_class = File::open("java-assets/compiled-classes/UnicodeStrings.class").unwrap();
    valid_class.read_to_end(&mut contents).unwrap();
    let res = ClassFile::read(&mut Cursor::new(contents));
    match res {
        Result::Ok(c) => {
            let mut found_utf_maths_string = false;
            let mut found_utf_runes_string = false;
            let mut found_utf_braille_string = false;
            let mut found_utf_modified_string = false;
            let mut found_utf_unpaired_string = false;

            if let ConstantInfo::Utf8(ref con) = c.const_pool[13] {
                assert_eq!(con.utf8_string, "2H₂ + O₂ ⇌ 2H₂O, R = 4.7 kΩ, ⌀ 200 mm");
            }

            if let ConstantInfo::Utf8(ref con) = c.const_pool[21] {
                assert_eq!(
                    con.utf8_string,
                    "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"
                );
            }

            if let ConstantInfo::Utf8(ref con) = c.const_pool[23] {
                assert_eq!(con.utf8_string, "⡌⠁⠧⠑ ⠼⠁⠒  ⡍⠜⠇⠑⠹⠰⠎ ⡣⠕⠌");
            }

            if let ConstantInfo::Utf8(ref con) = c.const_pool[25] {
                assert_eq!(con.utf8_string, "\0𠜎");
            }

            if let ConstantInfo::Utf8(ref con) = c.const_pool[27] {
                assert_eq!(con.utf8_string, "X���X");
                assert_eq!(con.utf8_string.len(), 11);
            }

            for (const_index, const_item) in c.const_pool.iter().enumerate() {
                println!("\t[{}] = {:?}", (const_index + 1), const_item);
            }
        }

        _ => panic!("Not a class file"),
    }
}

#[test]
fn test_malformed_class() {
    let malformed_class = include_bytes!("../java-assets/compiled-classes/malformed.class");
    let res = class_parser(malformed_class);
    if let Result::Ok((_, _)) = res {
        panic!("The file is not valid and shouldn't be parsed")
    };
}

#[test]
fn test_binrw_parse() {
    use binrw::prelude::*;
    use classfile_parser::ClassFile;
    use std::fs::File;
    use std::io::prelude::*;
    let mut contents: Vec<u8> = Vec::new();
    let mut regular_class = File::open("java-assets/compiled-classes/BasicClass.class").unwrap();
    regular_class.read_to_end(&mut contents).unwrap();
    let res = ClassFile::read(&mut Cursor::new(contents));
    match res {
        Result::Ok(c) => {
            println!(
                "Valid class file, version {},{} const_pool({}), this=const[{}], super=const[{}], interfaces({}), fields({}), methods({}), attributes({}), access({:?})",
                c.major_version,
                c.minor_version,
                c.const_pool_size,
                c.this_class,
                c.super_class,
                c.interfaces_count,
                c.fields_count,
                c.methods_count,
                c.attributes_count,
                c.access_flags
            );

            let mut code_const_index = 0;

            println!("Constant pool:");
            for (const_index, const_item) in c.const_pool.iter().enumerate() {
                println!("\t[{}] = {:?}", (const_index + 1), const_item);
                if let ConstantInfo::Utf8(ref c) = *const_item
                    && c.utf8_string.to_string() == "Code"
                {
                    code_const_index = (const_index + 1) as u16;
                }
            }
            println!("Code index = {}", code_const_index);

            println!("Interfaces:");
            for (interface_index, i) in c.interfaces.iter().enumerate() {
                println!(
                    "\t[{}] = const[{}] = {:?}",
                    interface_index,
                    i,
                    c.const_pool[(i - 1) as usize]
                );
            }
            println!("Fields:");
            for (field_index, f) in c.fields.iter().enumerate() {
                println!(
                    "\t[{}] Name(const[{}] = {:?}) - access({:?})",
                    field_index,
                    f.name_index,
                    c.const_pool[(f.name_index - 1) as usize],
                    f.access_flags
                );
            }
            println!("Methods:");
            for (method_index, m) in c.methods.iter().enumerate() {
                println!(
                    "\t[{}] Name(const[{}] = {:?}) - access({:?})",
                    method_index,
                    m.name_index,
                    c.const_pool[(m.name_index - 1) as usize],
                    m.access_flags
                );

                for a in &m.attributes {
                    if a.attribute_name_index == code_const_index {
                        println!("\t\tCode attr found, len = {}", a.attribute_length);
                        let code_result =
                            classfile_parser::attribute_info::code_attribute_parser(&a.info);
                        match code_result {
                            Result::Ok((_, code)) => {
                                println!("\t\t\tCode! code_length = {}", code.code_length);
                            }
                            _ => panic!("Not a valid code attr?"),
                        }
                    } else {
                        println!("\t\tAttribute: {:?}", a);
                    }
                }
            }
        }
        _ => panic!("Not a class file"),
    };
}

// #[test]
// fn test_constant_utf8() {
//     let hello_world_data = &[
//         // 0x01, // tag = 1
//         0x00, 0x0C, // length = 12
//         0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x57, 0x6F, 0x72, 0x6C, 0x64, 0x21 // 'Hello world!' in UTF8
//     ];
//     let res = const_utf8(hello_world_data);

//     match res {
//         Result::Ok((_, c)) =>
//         match c {
//             Constant::Utf8(ref s) =>
//                  println!("Valid UTF8 const: {}", s.utf8_string),
//             _ => panic!("It's a const, but of what type?")
//         },
//         _ => panic!("Not a UTF type const?"),
//     };
// }
