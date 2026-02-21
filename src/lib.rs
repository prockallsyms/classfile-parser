//! A parser for [Java Classfiles](https://docs.oracle.com/javase/specs/jvms/se10/html/jvms-4.html)

#[macro_use]
extern crate bitflags;

pub mod attribute_info;
pub mod constant_info;
pub mod field_info;
pub mod method_info;

pub mod code_attribute;

pub mod types;

pub use types::*;

#[cfg(feature = "jar-utils")]
pub mod jar_utils;

#[cfg(feature = "spring-utils")]
pub mod spring_utils;
