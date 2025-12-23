use crate::attribute_info::AttributeInfo;
use crate::constant_info::ConstantInfo;
use crate::field_info::FieldInfo;
use crate::method_info::MethodInfo;

use binrw::{
    binrw,
    meta::{EndianKind, ReadEndian},
    BinRead, BinWrite, Endian, VecArgs,
};

#[derive(BinWrite, Clone, Debug)]
#[brw(big, magic = b"\xca\xfe\xba\xbe")]
pub struct ClassFile {
    pub minor_version: u16,
    pub major_version: u16,
    pub const_pool_size: u16,
    pub const_pool: Vec<ConstantInfo>,
    pub access_flags: ClassAccessFlags,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces_count: u16,
    pub interfaces: Vec<u16>,
    pub fields_count: u16,
    pub fields: Vec<FieldInfo>,
    pub methods_count: u16,
    pub methods: Vec<MethodInfo>,
    pub attributes_count: u16,
    pub attributes: Vec<AttributeInfo>,
}

pub trait InterpretInner {
    fn interpret_inner(&mut self, const_pool: &Vec<ConstantInfo>);
}

impl ReadEndian for ClassFile {
    const ENDIAN: EndianKind = EndianKind::Endian(Endian::Big);
}

impl BinRead for ClassFile {
    type Args<'a> = ();

    fn read_options<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _endian: binrw::Endian,
        _args: Self::Args<'_>,
    ) -> binrw::BinResult<Self> {
        let magic = u32::read_options(reader, Endian::Big, ())?;
        if magic != u32::from_be_bytes([0xca, 0xfe, 0xba, 0xbe]) {
            return Err(binrw::Error::BadMagic { pos: 0, found: Box::new(magic) })
        }

        let minor_version = u16::read_options(reader, Endian::Big, ())?;
        let major_version = u16::read_options(reader, Endian::Big, ())?;
        let const_pool_size = u16::read_options(reader, Endian::Big, ())?;
        let const_pool = Vec::<ConstantInfo>::read_options(
            reader,
            Endian::Big,
            VecArgs {
                count: const_pool_size as usize,
                inner: (),
            },
        )?;
        let access_flags = ClassAccessFlags::read_options(reader, Endian::Big, ())?;
        dbg!(&access_flags);
        let this_class = u16::read_options(reader, Endian::Big, ())?;
        dbg!(&this_class);
        let super_class = u16::read_options(reader, Endian::Big, ())?;
        dbg!(&super_class);
        let interfaces_count = u16::read_options(reader, Endian::Big, ())?;
        dbg!(&interfaces_count);
        let interfaces = Vec::<u16>::read_options(
            reader,
            Endian::Big,
            VecArgs {
                count: interfaces_count as usize,
                inner: (),
            },
        )?;
        let fields_count = u16::read_options(reader, Endian::Big, ())?;
        dbg!(&fields_count);
        let mut fields = Vec::<FieldInfo>::read_options(
            reader,
            Endian::Big,
            VecArgs {
                count: fields_count as usize,
                inner: (),
            },
        )?;

        let methods_count = u16::read_options(reader, Endian::Big, ())?;
        let mut methods = Vec::<MethodInfo>::read_options(
            reader,
            Endian::Big,
            VecArgs {
                count: methods_count as usize,
                inner: (),
            },
        )?;

        let attributes_count = u16::read_options(reader, Endian::Big, ())?;
        let mut attributes = Vec::<AttributeInfo>::read_options(
            reader,
            Endian::Big,
            VecArgs {
                count: attributes_count as usize,
                inner: (),
            },
        )?;
        
        for field in &mut fields {
            field.interpret_inner(&const_pool);
        }

        for method in &mut methods {
            method.interpret_inner(&const_pool);
        }

        for attr in &mut attributes {
            attr.interpret_inner(&const_pool);
        }

        Ok(ClassFile {
            minor_version,
            major_version,
            const_pool_size,
            const_pool,
            access_flags,
            this_class,
            super_class,
            interfaces_count,
            interfaces,
            fields_count,
            fields,
            methods_count,
            methods,
            attributes_count,
            attributes,
        })
    }
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
