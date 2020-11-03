#[repr(u8)]
enum ConstantEntry {
    UTF8(String) = 1,
    Integer(i32) = 3,
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    String(u16),
    Field(u16, u16),
    Method(u16, u16),
    InterfaceMethod(u16, u16),
    NameAndType(u16, u16),
    MethodHandle(u8, u16) = 15,
    MethodType(u16),
    Dynamic(u16, u16),
    InvokeDynamic(u16, u16),
    Module(u16),
    Package(u16)
}
