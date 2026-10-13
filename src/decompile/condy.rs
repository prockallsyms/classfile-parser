//! Resolution of `CONSTANT_Dynamic` ("condy", JVMS §4.4.10) constant pool entries
//! loaded with `ldc` / `ldc_w` / `ldc2_w`.
//!
//! A dynamic constant is produced at link time by a bootstrap method. When the
//! bootstrap is one of the well-known `java.lang.invoke.ConstantBootstraps`
//! methods, the constant is rewritten into the plain Java expression it stands
//! for (`null`, `int.class`, `(long)42`, `Color.RED`, ...). Anything else is kept
//! as [`Expr::DynamicConstant`] and rendered with a `/* dynamic #n */` marker.

use crate::attribute_info::BootstrapMethod;
use crate::constant_info::{ConstantInfo, DynamicConstant};

use super::descriptor::{
    JvmType, internal_to_source_name, parse_method_descriptor, parse_type_descriptor,
};
use super::expr::{Expr, InvokeKind};
use super::util;

/// Bootstrap arguments may themselves be dynamic constants; stop unfolding at this depth.
const MAX_NESTING: u8 = 8;

const CONSTANT_BOOTSTRAPS: &str = "java/lang/invoke/ConstantBootstraps";

// MethodHandle reference kinds (JVMS §5.4.3.5).
const REF_INVOKE_VIRTUAL: u8 = 5;
const REF_INVOKE_STATIC: u8 = 6;
const REF_INVOKE_SPECIAL: u8 = 7;
const REF_NEW_INVOKE_SPECIAL: u8 = 8;
const REF_INVOKE_INTERFACE: u8 = 9;

/// Build the expression for loading the dynamic constant at 1-based pool `index`.
pub(super) fn load_dynamic_constant(
    const_pool: &[ConstantInfo],
    bootstrap_methods: &[BootstrapMethod],
    dynamic: &DynamicConstant,
    index: u16,
    depth: u8,
) -> Expr {
    let Some((name, descriptor)) = util::get_name_and_type(const_pool, dynamic.name_and_type_index)
    else {
        return Expr::Unresolved(format!("dynamic_cp#{}", index));
    };
    let value_type = parse_type_descriptor(descriptor).unwrap_or(JvmType::Unknown);

    if depth < MAX_NESTING
        && let Some(expr) = reduce_known_bootstrap(
            const_pool,
            bootstrap_methods,
            dynamic,
            name,
            &value_type,
            depth,
        )
    {
        return expr;
    }

    Expr::DynamicConstant {
        bootstrap_index: dynamic.bootstrap_method_attr_index,
        name: name.to_string(),
        descriptor: descriptor.to_string(),
        value_type,
    }
}

/// Rewrite a constant bootstrapped by `java.lang.invoke.ConstantBootstraps` into
/// the Java expression it denotes. Returns `None` for any other bootstrap or shape.
fn reduce_known_bootstrap(
    const_pool: &[ConstantInfo],
    bootstrap_methods: &[BootstrapMethod],
    dynamic: &DynamicConstant,
    name: &str,
    value_type: &JvmType,
    depth: u8,
) -> Option<Expr> {
    let bsm = bootstrap_methods.get(dynamic.bootstrap_method_attr_index as usize)?;
    let (kind, owner, method, _) = resolve_method_handle(const_pool, bsm.bootstrap_method_ref)?;
    if kind != REF_INVOKE_STATIC || owner != CONSTANT_BOOTSTRAPS {
        return None;
    }
    let args = &bsm.bootstrap_arguments;
    let arg = |i: usize| -> Option<Expr> {
        args.get(i)
            .map(|&a| static_argument(const_pool, bootstrap_methods, a, depth + 1))
    };

    match (method, args.len()) {
        ("nullConstant", 0) => Some(Expr::NullLiteral),
        ("primitiveClass", 0) => Some(Expr::ClassLiteral(primitive_name(name)?.to_string())),
        ("explicitCast", 1) => Some(Expr::Cast {
            target_type: value_type.clone(),
            operand: Box::new(arg(0)?),
        }),
        // getStaticFinal(Lookup, name, type) and enumConstant(Lookup, name, type):
        // the field's declaring class is the constant's own type.
        ("getStaticFinal", 0) | ("enumConstant", 0) => {
            let class_name = reference_class(value_type)?;
            Some(field_get(class_name, name, value_type))
        }
        // getStaticFinal(Lookup, name, type, declaringClass)
        ("getStaticFinal", 1) => {
            let class_name = util::get_class_name(const_pool, args[0])?;
            Some(field_get(class_name, name, value_type))
        }
        // invoke(Lookup, name, type, MethodHandle handle, Object... args)
        ("invoke", n) if n >= 1 => {
            let (kind, owner, method, descriptor) = resolve_method_handle(const_pool, args[0])?;
            let call_args = (1..n).map(arg).collect::<Option<Vec<_>>>()?;
            invoke_expr(kind, owner, method, descriptor, call_args)
        }
        _ => None,
    }
}

/// Resolve a MethodHandle constant to (reference_kind, owner class, name, descriptor).
fn resolve_method_handle(
    const_pool: &[ConstantInfo],
    index: u16,
) -> Option<(u8, &str, &str, &str)> {
    match const_pool.get((index as usize).checked_sub(1)?)? {
        ConstantInfo::MethodHandle(h) => {
            let (owner, name, descriptor) = util::resolve_ref(const_pool, h.reference_index)?;
            Some((h.reference_kind, owner, name, descriptor))
        }
        _ => None,
    }
}

/// The expression for a `ConstantBootstraps.invoke` target with its static arguments.
fn invoke_expr(
    kind: u8,
    owner: &str,
    method: &str,
    descriptor: &str,
    mut args: Vec<Expr>,
) -> Option<Expr> {
    let (_, return_type) =
        parse_method_descriptor(descriptor).unwrap_or_else(|| (vec![], JvmType::Unknown));
    match kind {
        REF_INVOKE_STATIC => Some(Expr::MethodCall {
            kind: InvokeKind::Static,
            object: None,
            class_name: owner.to_string(),
            method_name: method.to_string(),
            descriptor: descriptor.to_string(),
            args,
            return_type,
        }),
        REF_NEW_INVOKE_SPECIAL => Some(Expr::New {
            class_name: owner.to_string(),
            constructor_descriptor: descriptor.to_string(),
            args,
        }),
        REF_INVOKE_VIRTUAL | REF_INVOKE_SPECIAL | REF_INVOKE_INTERFACE => {
            if args.is_empty() {
                return None;
            }
            let receiver = args.remove(0);
            let kind = match kind {
                REF_INVOKE_INTERFACE => InvokeKind::Interface,
                REF_INVOKE_SPECIAL => InvokeKind::Special,
                _ => InvokeKind::Virtual,
            };
            Some(Expr::MethodCall {
                kind,
                object: Some(Box::new(receiver)),
                class_name: owner.to_string(),
                method_name: method.to_string(),
                descriptor: descriptor.to_string(),
                args,
                return_type,
            })
        }
        _ => None,
    }
}

/// A bootstrap method's static argument (a loadable constant) as an expression.
fn static_argument(
    const_pool: &[ConstantInfo],
    bootstrap_methods: &[BootstrapMethod],
    index: u16,
    depth: u8,
) -> Expr {
    match const_pool.get((index as usize).wrapping_sub(1)) {
        Some(ConstantInfo::Integer(c)) => Expr::IntLiteral(c.value),
        Some(ConstantInfo::Float(c)) => Expr::FloatLiteral(c.value),
        Some(ConstantInfo::Long(c)) => Expr::LongLiteral(c.value),
        Some(ConstantInfo::Double(c)) => Expr::DoubleLiteral(c.value),
        Some(ConstantInfo::String(c)) => match util::get_utf8(const_pool, c.string_index) {
            Some(s) => Expr::StringLiteral(s.to_string()),
            None => Expr::Unresolved(format!("string_cp#{}", c.string_index)),
        },
        Some(ConstantInfo::Class(c)) => match util::get_utf8(const_pool, c.name_index) {
            Some(n) => Expr::ClassLiteral(internal_to_source_name(n)),
            None => Expr::Unresolved(format!("class_cp#{}", c.name_index)),
        },
        Some(ConstantInfo::Dynamic(d)) => {
            load_dynamic_constant(const_pool, bootstrap_methods, d, index, depth)
        }
        _ => Expr::Unresolved(format!("cp#{}", index)),
    }
}

fn field_get(class_name: &str, field_name: &str, field_type: &JvmType) -> Expr {
    Expr::FieldGet {
        object: None,
        class_name: class_name.to_string(),
        field_name: field_name.to_string(),
        field_type: field_type.clone(),
    }
}

fn reference_class(ty: &JvmType) -> Option<&str> {
    match ty {
        JvmType::Reference(name) => Some(name.as_str()),
        _ => None,
    }
}

/// The Java keyword for a primitive type descriptor used as a `primitiveClass` name.
fn primitive_name(descriptor: &str) -> Option<&'static str> {
    Some(match descriptor {
        "I" => "int",
        "J" => "long",
        "F" => "float",
        "D" => "double",
        "Z" => "boolean",
        "B" => "byte",
        "C" => "char",
        "S" => "short",
        "V" => "void",
        _ => return None,
    })
}
