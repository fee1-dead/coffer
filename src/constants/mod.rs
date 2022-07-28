//! Constants that can be found in Java class files.

/// The magic header file that exists on top of every class file with java version > 1.0.2.
pub const JVM_MAGIC: u32 = 0xCAFEBABE;
pub mod insn;
pub mod target_types;
