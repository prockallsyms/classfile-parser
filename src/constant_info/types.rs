use binrw::{binrw, BinResult};
use std::fmt::Debug;

#[derive(Clone, Debug)]
#[binrw]
pub enum ConstantInfo {
    #[br(magic(1u8))]
    Utf8(Utf8Constant),
    #[br(magic(3u8))]
    Integer(IntegerConstant),
    #[br(magic(4u8))]
    Float(FloatConstant),
    #[br(magic(5u8))]
    Long(LongConstant),
    #[br(magic(6u8))]
    Double(DoubleConstant),
    #[br(magic(7u8))]
    Class(ClassConstant),
    #[br(magic(8u8))]
    String(StringConstant),
    #[br(magic(9u8))]
    FieldRef(FieldRefConstant),
    #[br(magic(10u8))]
    MethodRef(MethodRefConstant),
    #[br(magic(11u8))]
    InterfaceMethodRef(InterfaceMethodRefConstant),
    #[br(magic(12u8))]
    NameAndType(NameAndTypeConstant),
    #[br(magic(15u8))]
    MethodHandle(MethodHandleConstant),
    #[br(magic(16u8))]
    MethodType(MethodTypeConstant),
    #[br(magic(18u8))]
    InvokeDynamic(InvokeDynamicConstant),
    Unusable,
}

#[binrw::parser(reader, endian)]
pub fn string_reader() -> BinResult<String> {
    let len = u16::from_be_bytes(reader.read_array()?);
    let mut string_bytes = vec![0; len as usize];
    let _ = reader.read_exact(&mut string_bytes);
    let utf8_string = cesu8::from_java_cesu8(&string_bytes)
        .unwrap_or_else(|_| String::from_utf8_lossy(&string_bytes));
    Ok(utf8_string.to_string())
}

#[binrw::writer(writer, endian)]
pub fn string_writer<'a>(s: &'a String) -> BinResult<()> {
    let _ = writer.write(&u16::to_be_bytes(s.len() as u16));
    writer.write_all(s.as_bytes())?;
    Ok(())
}

#[derive(Clone, Debug)]
#[binrw]
pub struct Utf8Constant {
    #[br(parse_with = crate::constant_info::string_reader)]
    #[bw(write_with = crate::constant_info::string_writer)]
    pub utf8_string: String,
    // pub bytes: Vec<u8>,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct IntegerConstant {
    pub value: i32,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct FloatConstant {
    pub value: f32,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct LongConstant {
    pub value: i64,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct DoubleConstant {
    pub value: f64,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct ClassConstant {
    pub name_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct StringConstant {
    pub string_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct FieldRefConstant {
    pub class_index: u16,
    pub name_and_type_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct MethodRefConstant {
    pub class_index: u16,
    pub name_and_type_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct InterfaceMethodRefConstant {
    pub class_index: u16,
    pub name_and_type_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct NameAndTypeConstant {
    pub name_index: u16,
    pub descriptor_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct MethodHandleConstant {
    pub reference_kind: u8,
    pub reference_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct MethodTypeConstant {
    pub descriptor_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct InvokeDynamicConstant {
    pub bootstrap_method_attr_index: u16,
    pub name_and_type_index: u16,
}
