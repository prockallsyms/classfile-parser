use std::io::Cursor;

use binrw::{binrw, BinRead, BinResult};

use crate::{
    code_attribute::{LocalVariableTableAttribute, LocalVariableTypeTableAttribute},
    constant_info::{ConstantInfo, Utf8Constant},
    InterpretInner,
};

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct AttributeInfo {
    pub attribute_name_index: u16,
    pub attribute_length: u32,
    #[br(count = attribute_length)]
    info: Vec<u8>,
    #[brw(ignore)]
    pub info_parsed: Option<AttributeInfoVariant>,
}

impl InterpretInner for AttributeInfo {
    fn interpret_inner(&mut self, constant_pool: &Vec<ConstantInfo>) {
        assert!(self.info_parsed.is_none(), "parsing has already happened");

        assert_eq!(
            self.info.len(),
            self.attribute_length as usize,
            "mismatched attr length and info vec length! {} != {}",
            self.info.len(),
            self.attribute_length
        );

        match &constant_pool[self.attribute_name_index as usize] {
            ConstantInfo::Utf8(Utf8Constant {
                utf8_string: attr_name,
            }) => {
                self.info_parsed = match attr_name.as_str() {
                    "ConstantValue" => {
                        let c =
                            ConstantValueAttribute::read(&mut Cursor::new(&mut self.info)).unwrap();
                        Some(AttributeInfoVariant::ConstantValue(c))
                    }
                    "Code" => {
                        let c = CodeAttribute::read(&mut Cursor::new(&mut self.info)).unwrap();
                        Some(AttributeInfoVariant::Code(c))
                    }
                    "StackMapTable" => {
                        let c =
                            StackMapTableAttribute::read(&mut Cursor::new(&mut self.info)).unwrap();
                        Some(AttributeInfoVariant::StackMapTable(c))
                    }
                    "BootstrapMethods" => {
                        let c = BootstrapMethodsAttribute::read(&mut Cursor::new(&mut self.info))
                            .unwrap();
                        Some(AttributeInfoVariant::BootstrapMethods(c))
                    }
                    "Exceptions" => {
                        let c =
                            ExceptionsAttribute::read(&mut Cursor::new(&mut self.info)).unwrap();
                        Some(AttributeInfoVariant::Exceptions(c))
                    }
                    "InnerClasses" => {
                        let c =
                            InnerClassesAttribute::read(&mut Cursor::new(&mut self.info)).unwrap();
                        Some(AttributeInfoVariant::InnerClasses(c))
                    }
                    "EnclosingMethod" => {
                        let c = EnclosingMethodAttribute::read(&mut Cursor::new(&mut self.info))
                            .unwrap();
                        Some(AttributeInfoVariant::EnclosingMethod(c))
                    }
                    "Synthetic" => {
                        let c = SyntheticAttribute::read(&mut Cursor::new(&mut self.info)).unwrap();
                        Some(AttributeInfoVariant::Synthetic(c))
                    }
                    "Signature" => {
                        let c = SignatureAttribute::read(&mut Cursor::new(&mut self.info)).unwrap();
                        Some(AttributeInfoVariant::Signature(c))
                    }
                    "SourceFile" => {
                        let c =
                            SourceFileAttribute::read(&mut Cursor::new(&mut self.info)).unwrap();
                        Some(AttributeInfoVariant::SourceFile(c))
                    }
                    "LineNumberTable" => {
                        let c = LineNumberTableAttribute::read(&mut Cursor::new(&mut self.info))
                            .unwrap();
                        Some(AttributeInfoVariant::LineNumberTable(c))
                    }
                    "LocalVariableTable" => {
                        let c = LocalVariableTableAttribute::read(&mut Cursor::new(&mut self.info))
                            .unwrap();
                        Some(AttributeInfoVariant::LocalVariableTable(c))
                    }
                    "LocalVariableTypeTable" => {
                        let c =
                            LocalVariableTypeTableAttribute::read(&mut Cursor::new(&mut self.info))
                                .unwrap();
                        Some(AttributeInfoVariant::LocalVariableTypeTable(c))
                    }
                    "SourceDebugExtension" => {
                        let c =
                            SourceDebugExtensionAttribute::read(&mut Cursor::new(&mut self.info))
                                .unwrap();
                        Some(AttributeInfoVariant::SourceDebugExtension(c))
                    }
                    "Deprecated" => {
                        let c =
                            DeprecatedAttribute::read(&mut Cursor::new(&mut self.info)).unwrap();
                        Some(AttributeInfoVariant::Deprecated(c))
                    }
                    "RuntimeVisibleAnnotations" => {
                        let c = RuntimeVisibleAnnotationsAttribute::read(&mut Cursor::new(
                            &mut self.info,
                        ))
                        .unwrap();
                        Some(AttributeInfoVariant::RuntimeVisibleAnnotations(c))
                    }
                    "RuntimeInvisibleAnnotations" => {
                        let c = RuntimeInvisibleAnnotationsAttribute::read(&mut Cursor::new(
                            &mut self.info,
                        ))
                        .unwrap();
                        Some(AttributeInfoVariant::RuntimeInvisibleAnnotations(c))
                    }
                    "RuntimeVisibleParameterAnnotations" => {
                        let c = RuntimeVisibleParameterAnnotationsAttribute::read(
                            &mut Cursor::new(&mut self.info),
                        )
                        .unwrap();
                        Some(AttributeInfoVariant::RuntimeVisibleParameterAnnotations(c))
                    }
                    "RuntimeInvisibleParameterAnnotations" => {
                        let c = RuntimeInvisibleParameterAnnotationsAttribute::read(
                            &mut Cursor::new(&mut self.info),
                        )
                        .unwrap();
                        Some(AttributeInfoVariant::RuntimeInvisibleParameterAnnotations(
                            c,
                        ))
                    }
                    "RuntimeVisibleTypeAnnotations" => {
                        let c = RuntimeVisibleTypeAnnotationsAttribute::read(&mut Cursor::new(
                            &mut self.info,
                        ))
                        .unwrap();
                        Some(AttributeInfoVariant::RuntimeVisibleTypeAnnotations(c))
                    }
                    "RuntimeInvisibleTypeAnnotations" => {
                        let c = RuntimeInvisibleTypeAnnotationsAttribute::read(&mut Cursor::new(
                            &mut self.info,
                        ))
                        .unwrap();
                        Some(AttributeInfoVariant::RuntimeInvisibleTypeAnnotations(c))
                    }
                    "AnnotationDefault" => {
                        let c = AnnotationDefaultAttribute::read(&mut Cursor::new(&mut self.info))
                            .unwrap();
                        Some(AttributeInfoVariant::AnnotationDefault(c))
                    }
                    "MethodParameters" => {
                        let c = MethodParametersAttribute::read(&mut Cursor::new(&mut self.info))
                            .unwrap();
                        Some(AttributeInfoVariant::MethodParameters(c))
                    }
                    /*
                    "Module" => panic!(
                    "ModulePackage" => {},
                    "ModuleMainClass" => {},
                    */
                    _ => panic!("Unhandled attribute type"),
                }
            }
            _ => panic!(
                "attribute info name index points to non-UTF8 constant value in the constant_pool"
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AttributeInfoVariant {
    ConstantValue(ConstantValueAttribute),
    Code(CodeAttribute),
    StackMapTable(StackMapTableAttribute),
    Exceptions(ExceptionsAttribute),
    InnerClasses(InnerClassesAttribute),
    EnclosingMethod(EnclosingMethodAttribute),
    Synthetic(SyntheticAttribute),
    Signature(SignatureAttribute),
    SourceFile(SourceFileAttribute),
    SourceDebugExtension(SourceDebugExtensionAttribute),
    LineNumberTable(LineNumberTableAttribute),
    LocalVariableTable(LocalVariableTableAttribute),
    LocalVariableTypeTable(LocalVariableTypeTableAttribute),
    Deprecated(DeprecatedAttribute),
    RuntimeVisibleAnnotations(RuntimeVisibleAnnotationsAttribute),
    RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotationsAttribute),
    RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotationsAttribute),
    RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotationsAttribute),
    RuntimeVisibleTypeAnnotations(RuntimeVisibleTypeAnnotationsAttribute),
    RuntimeInvisibleTypeAnnotations(RuntimeInvisibleTypeAnnotationsAttribute),
    AnnotationDefault(AnnotationDefaultAttribute),
    BootstrapMethods(BootstrapMethodsAttribute),
    MethodParameters(MethodParametersAttribute),
}

#[derive(Clone, Debug)]
#[binrw]
pub struct ExceptionEntry {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: u16,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct CodeAttribute {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code_length: u32,
    #[br(count = code_length)]
    pub code: Vec<u8>,
    pub exception_table_length: u16,
    #[br(count = exception_table_length)]
    pub exception_table: Vec<ExceptionEntry>,
    pub attributes_count: u16,
    #[br(count = attributes_count)]
    pub attributes: Vec<AttributeInfo>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct MethodParametersAttribute {
    pub parameters_count: u8,
    #[br(count = parameters_count)]
    pub parameters: Vec<ParameterAttribute>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct ParameterAttribute {
    pub name_index: u16,
    pub access_flags: u16,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct InnerClassesAttribute {
    pub number_of_classes: u16,
    #[br(count = number_of_classes)]
    pub classes: Vec<InnerClassInfo>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct InnerClassInfo {
    pub inner_class_info_index: u16,
    pub outer_class_info_index: u16,
    pub inner_name_index: u16,
    pub inner_class_access_flags: u16,
}

bitflags! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub struct InnerClassAccessFlags: u16 {
        const PUBLIC = 0x0001;     //	Declared public; may be accessed from outside its package.
        const PRIVATE = 0x0002;    //	Declared private; may not be accessed from outside its package.
        const PROTECTED = 0x0004;  //	Declared praotected; may only be accessed within children.
        const STATIC = 0x0008;     //	Declared static.
        const FINAL = 0x0010;      //	Declared final; no subclasses allowed.
        const INTERFACE = 0x0200;  //	Is an interface, not a class.
        const ABSTRACT = 0x0400;   //	Declared abstract; must not be instantiated.
        const SYNTHETIC = 0x1000;  //	Declared synthetic; not present in the source code.
        const ANNOTATION = 0x2000; //	Declared as an annotation type.
        const ENUM = 0x4000;       //	Declared as an enum type.
    }
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct EnclosingMethodAttribute {
    pub class_index: u16,
    pub method_index: u16,
}

// in all reality this struct isn't required b/c it's zero sized
// "Deprecated" is a marker attribute
#[derive(Clone, Debug)]
#[binrw]
pub struct DeprecatedAttribute {}

// in all reality this struct isn't required b/c it's zero sized
// "Synthetic" is a marker attribute
#[derive(Clone, Debug)]
#[binrw]
pub struct SyntheticAttribute {}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct SignatureAttribute {
    pub signature_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct RuntimeVisibleAnnotationsAttribute {
    pub num_annotations: u16,
    #[br(count = num_annotations)]
    pub annotations: Vec<RuntimeAnnotation>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct RuntimeInvisibleAnnotationsAttribute {
    pub num_annotations: u16,
    #[br(count = num_annotations)]
    pub annotations: Vec<RuntimeAnnotation>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct RuntimeVisibleParameterAnnotationsAttribute {
    pub num_parameters: u8,
    #[br(count = num_parameters)]
    pub parameter_annotations: Vec<RuntimeVisibleAnnotationsAttribute>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct RuntimeInvisibleParameterAnnotationsAttribute {
    pub num_parameters: u8,
    #[br(count = num_parameters)]
    pub parameter_annotations: Vec<RuntimeInvisibleAnnotationsAttribute>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct RuntimeVisibleTypeAnnotationsAttribute {
    pub num_annotations: u16,
    #[br(count = num_annotations)]
    pub type_annotations: Vec<TypeAnnotation>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct RuntimeInvisibleTypeAnnotationsAttribute {
    pub num_annotations: u16,
    #[br(count = num_annotations)]
    pub type_annotations: Vec<TypeAnnotation>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct TypeAnnotation {
    pub target_type: u8,
    pub target_info: TargetInfo,
    pub target_path: TypePath,
    pub type_index: u16,
    pub num_element_value_pairs: u16,
    #[br(count = num_element_value_pairs)]
    pub element_value_pairs: Vec<ElementValuePair>,
}

#[derive(Clone, Debug)]
#[binrw]
pub enum TargetInfo {
    TypeParameter {
        type_parameter_index: u8,
    },
    SuperType {
        supertype_index: u16,
    },
    TypeParameterBound {
        type_parameter_index: u8,
        bound_index: u8,
    },
    Empty,
    FormalParameter {
        formal_parameter_index: u8,
    },
    Throws {
        throws_type_index: u16,
    },
    LocalVar {
        table_length: u16,
        #[br(count = table_length)]
        tables: Vec<LocalVarTableAnnotation>,
    },
    Catch {
        exception_table_index: u16,
    },
    Offset {
        offset: u16,
    },
    TypeArgument {
        offset: u16,
        type_argument_index: u8,
    },
}

#[derive(Clone, Debug)]
#[binrw]
pub struct TypePath {
    pub path_length: u8,
    #[br(count = path_length)]
    pub paths: Vec<TypePathEntry>,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct TypePathEntry {
    pub type_path_kind: u8,
    pub type_argument_index: u8,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct LocalVarTableAnnotation {
    pub start_pc: u16,
    pub length: u16,
    pub index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct RuntimeAnnotation {
    pub type_index: u16,
    pub num_element_value_pairs: u16,
    #[br(count = num_element_value_pairs)]
    pub element_value_pairs: Vec<ElementValuePair>,
}

pub type AnnotationDefaultAttribute = ElementValue;

#[derive(Clone, Debug)]
#[binrw]
pub struct ElementValuePair {
    pub element_name_index: u16,
    pub value: ElementValue,
}

#[binrw::parser(reader)]
fn custom_char_parser() -> BinResult<char> {
    let c = u8::from_be_bytes(reader.read_array()?) as char;
    Ok(c)
}

#[binrw::writer(writer)]
pub fn custom_char_writer(c: &char) -> BinResult<()> {
    writer.write_all(c.to_string().as_bytes())?;
    Ok(())
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub enum ElementValue {
    // pub tag: u8,
    ConstValueIndex(ConstValueIndexValue),
    EnumConst(EnumConstValue),
    ClassInfoIndex(u16),
    AnnotationValue(RuntimeAnnotation),
    ElementArray(ElementArrayValue),
}

#[derive(Clone, Debug)]
#[binrw]
pub struct ConstValueIndexValue {
    #[br(parse_with = custom_char_parser)]
    #[bw(write_with = custom_char_writer)]
    tag: char,
    value: u16,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct ElementArrayValue {
    pub num_values: u16,
    #[br(count = num_values)]
    pub values: Vec<ElementValue>,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct EnumConstValue {
    pub type_name_index: u16,
    pub const_name_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct SourceDebugExtensionAttribute {
    // Per the spec:
    // The debug_extension array holds extended debugging information which has no
    // semantic effect on the Java Virtual Machine. The information is represented
    // using a modified UTF-8 string with no terminating zero byte.
    // pub debug_extension: Vec<u8>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct LineNumberTableAttribute {
    pub line_number_table_length: u16,
    #[br(count = line_number_table_length)]
    pub line_number_table: Vec<LineNumberTableEntry>,
}

#[derive(Clone, Debug)]
#[binrw]
pub struct LineNumberTableEntry {
    pub start_pc: u16,
    pub line_number: u16,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub enum VerificationTypeInfo {
    #[br(magic = 0u8)]
    Top,
    #[br(magic = 1u8)]
    Integer,
    #[br(magic = 2u8)]
    Float,
    #[br(magic = 3u8)]
    Double,
    #[br(magic = 4u8)]
    Long,
    #[br(magic = 5u8)]
    Null,
    #[br(magic = 6u8)]
    UninitializedThis,
    #[br(magic = 7u8)]
    Object {
        /// An index into the constant pool for the class of the object
        class: u16,
    },
    #[br(magic = 8u8)]
    Uninitialized {
        /// Offset into associated code array of a new instruction
        /// that created the object being stored here.
        offset: u16,
    },
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct StackMapFrame {
    frame_type: u8,
    #[br(args(frame_type))]
    inner: StackMapFrameInner,
}

#[derive(Clone, Debug)]
#[binrw]
#[br(import(frame_type: u8))]
pub enum StackMapFrameInner {
    #[br(pre_assert((0..=63).contains(&frame_type)))]
    SameFrame {
        //frame_type: u8,
    },
    #[br(pre_assert((64..=127).contains(&frame_type)))]
    SameLocals1StackItemFrame {
        //frame_type: u8,
        stack: VerificationTypeInfo,
    },
    #[br(pre_assert(frame_type == 247))]
    SameLocals1StackItemFrameExtended {
        //frame_type: u8,
        offset_delta: u16,
        stack: VerificationTypeInfo,
    },
    #[br(pre_assert((248..=250).contains(&frame_type)))]
    ChopFrame {
        //frame_type: u8,
        offset_delta: u16,
    },
    #[br(pre_assert(frame_type == 251))]
    SameFrameExtended {
        //frame_type: u8,
        offset_delta: u16,
    },
    #[br(pre_assert((252..=254).contains(&frame_type)))]
    AppendFrame {
        //frame_type: u8,
        offset_delta: u16,
        #[br(count = frame_type - 251)]
        locals: Vec<VerificationTypeInfo>,
    },
    #[br(pre_assert(frame_type == 255))]
    FullFrame {
        //frame_type: u8,
        offset_delta: u16,
        number_of_locals: u16,
        #[br(count = number_of_locals)]
        locals: Vec<VerificationTypeInfo>,
        number_of_stack_items: u16,
        #[br(count = number_of_stack_items)]
        stack: Vec<VerificationTypeInfo>,
    },
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct StackMapTableAttribute {
    pub number_of_entries: u16,
    #[br(count = number_of_entries)]
    pub entries: Vec<StackMapFrame>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct ExceptionsAttribute {
    pub exception_table_length: u16,
    #[br(count = exception_table_length)]
    pub exception_table: Vec<u16>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct ConstantValueAttribute {
    pub constant_value_index: u16,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct BootstrapMethod {
    pub bootstrap_method_ref: u16,
    pub num_bootstrap_arguments: u16,
    #[br(count = num_bootstrap_arguments)]
    pub bootstrap_arguments: Vec<u16>,
}

#[derive(Clone, Debug)]
#[binrw]
#[brw(big)]
pub struct BootstrapMethodsAttribute {
    pub num_bootstrap_methods: u16,
    #[br(count = num_bootstrap_methods)]
    pub bootstrap_methods: Vec<BootstrapMethod>,
}

/// The SourceFile attribute is an optional fixed-length attribute in the attributes table of a ClassFile structure (§4.1).
///
/// There may be at most one SourceFile attribute in the attributes table of a ClassFile structure.
/// [see more](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.10)
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[binrw]
#[brw(big)]
pub struct SourceFileAttribute {
    /// The value of the sourcefile_index item must be a valid index into the constant_pool table.
    /// The constant_pool entry at that index must be a CONSTANT_Utf8_info structure representing a string.
    pub sourcefile_index: u16,
}
