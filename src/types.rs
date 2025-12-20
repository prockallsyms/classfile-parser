use crate::attribute_info::AttributeInfo;
use crate::constant_info::ConstantInfo;
use crate::field_info::FieldInfo;
use crate::method_info::MethodInfo;

use binrw::binrw;

#[derive(Clone, Debug)]
#[binrw]
#[brw(big, magic = b"\xca\xfe\xba\xbe")]
pub struct ClassFile {
    pub minor_version: u16,
    pub major_version: u16,
    pub const_pool_size: u16,
    #[br(count = const_pool_size)]
    pub const_pool: Vec<ConstantInfo>,
    pub access_flags: ClassAccessFlags,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces_count: u16,
    #[br(count = interfaces_count)]
    pub interfaces: Vec<u16>,
    pub fields_count: u16,
    #[br(count = fields_count)]
    pub fields: Vec<FieldInfo>,
    pub methods_count: u16,
    #[br(count = methods_count)]
    pub methods: Vec<MethodInfo>,
    pub attributes_count: u16,
    #[br(count = attributes_count)]
    pub attributes: Vec<AttributeInfo>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[binrw]
pub struct ClassAccessFlags(u16);

bitflags! {
    impl ClassAccessFlags: u16 {
        const PUBLIC = 0x0001;     //	Declared public; may be accessed from outside its package.
        const FINAL = 0x0010;      //	Declared final; no subclasses allowed.
        const SUPER = 0x0020;      //	Treat superclass methods specially when invoked by the invokespecial instruction.
        const INTERFACE = 0x0200;  //	Is an interface, not a class.
        const ABSTRACT = 0x0400;   //	Declared abstract; must not be instantiated.
        const SYNTHETIC = 0x1000;  //	Declared synthetic; not present in the source code.
        const ANNOTATION = 0x2000; //	Declared as an annotation type.
        const ENUM = 0x4000;       //	Declared as an enum type.
        const MODULE = 0x8000;     //	Declared as a module type.
    }
}
