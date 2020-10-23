

pub const JVM_MAGIC: u32 = 0xCAFEBABE;


bitflags! {
    pub struct AccessFlags: u16 {
        // @formatter:off
        const ACC_PUBLIC       = 0b00000000_00000001;
        const ACC_PRIVATE      = 0b00000000_00000010;
        const ACC_PROTECTED    = 0b00000000_00000100;
        const ACC_STATIC       = 0b00000000_00001000;
        const ACC_FINAL        = 0b00000000_00010000;
        const ACC_SUPER        = 0b00000000_00100000;
        const ACC_SYNCHRONIZED = 0b00000000_00100000;
        const ACC_OPEN         = 0b00000000_00100000;
        const ACC_TRANSITIVE   = 0b00000000_00100000;
        const ACC_VOLATILE     = 0b00000000_01000000;
        const ACC_BRIDGE       = 0b00000000_01000000;
        const ACC_STATIC_PHASE = 0b00000000_01000000;
        const ACC_VARARGS      = 0b00000000_10000000;
        const ACC_TRANSIENT    = 0b00000000_10000000;
        const ACC_NATIVE       = 0b00000001_00000000;
        const ACC_INTERFACE    = 0b00000010_00000000;
        const ACC_ABSTRACT     = 0b00000100_00000000;
        const ACC_STRICT       = 0b00001000_00000000;
        const ACC_SYNTHETIC    = 0b00010000_00000000;
        const ACC_ANNOTATION   = 0b00100000_00000000;
        const ACC_ENUM         = 0b01000000_00000000;
        const ACC_MANDATED     = 0b10000000_00000000;
        const ACC_MODULE       = 0b10000000_00000000;
        // @formatter:on
    }
}
