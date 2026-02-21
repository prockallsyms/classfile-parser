extern crate classfile_parser;

use std::fs::File;

use binrw::{BinRead, io::BufReader};
use classfile_parser::attribute_info::AttributeInfoVariant;
use classfile_parser::{ClassFile, constant_info::ConstantInfo};

#[test]
fn test_attribute_bootstrap_methods() {
    let bootstrap_methods_class =
        File::open("java-assets/compiled-classes/BootstrapMethods.class").unwrap();
    let res = ClassFile::read(&mut BufReader::new(bootstrap_methods_class));
    match res {
        Ok(c) => {
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

            let mut bootstrap_method_const_index = 0;

            println!("Constant pool:");
            for (const_index, const_item) in c.const_pool.iter().enumerate() {
                println!("\t[{}] = {:?}", (const_index + 1), const_item);
                if let ConstantInfo::Utf8(ref c) = *const_item
                    && c.utf8_string.to_string() == "BootstrapMethods"
                {
                    if bootstrap_method_const_index != 0 {
                        panic!("Should not find more than one BootstrapMethods constant");
                    }
                    bootstrap_method_const_index = (const_index + 1) as u16;
                }
            }
            assert_ne!(bootstrap_method_const_index, 0);

            println!(
                "Bootstrap Methods constant index = {}",
                bootstrap_method_const_index
            );

            for attribute_item in c.attributes.iter() {
                if attribute_item.attribute_name_index == bootstrap_method_const_index {
                    match attribute_item.info_parsed.as_ref() {
                        Some(AttributeInfoVariant::BootstrapMethods(bsma)) => {
                            assert_eq!(bsma.num_bootstrap_methods, 1);
                            let bsm = &bsma.bootstrap_methods[0];
                            assert_eq!(bsm.num_bootstrap_arguments, 3);
                            return;
                        }
                        _ => panic!("Failed to parse bootstrap method attribute"),
                    }
                }
            }

            panic!("Should not get to here");
        }
        _ => panic!("Not a valid class file"),
    }
}

#[test]
fn should_have_no_bootstrap_method_attr_if_no_invoke_dynamic() {
    let bootstrap_methods_class =
        File::open("java-assets/compiled-classes/BasicClass.class").unwrap();
    let res = ClassFile::read(&mut BufReader::new(bootstrap_methods_class));
    match res {
        Ok(c) => {
            for const_item in c.const_pool.iter() {
                if let ConstantInfo::Utf8(ref c) = *const_item
                    && c.utf8_string.to_string() == "BootstrapMethods"
                {
                    panic!(
                        "Should not have found a BootstrapMethods constant in a class not requiring it"
                    )
                }
            }
        }
        _ => panic!("Not a valid class file"),
    }
}
