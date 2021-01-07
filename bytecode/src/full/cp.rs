use std::hash::{Hash, Hasher};
use std::borrow::Cow;

#[derive(ReadWrite, Debug)]
#[tag_type(u8)]
pub enum RawConstantEntry {
    #[tag(1)]
    UTF8(Cow<'static, str>),
    #[tag(3)]
    Int(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    String(u16),
    Field(u16, u16),
    Method(u16, u16),
    InterfaceMethod(u16, u16),
    NameAndType(u16, u16),
    #[tag(15)]
    MethodHandle(u8, u16),
    MethodType(u16),
    Dynamic(u16, u16),
    InvokeDynamic(u16, u16),
    Module(u16),
    Package(u16)
}
impl Hash for RawConstantEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            RawConstantEntry::UTF8(ref s) => { s.hash(state) }
            RawConstantEntry::Int(ref i) => { i.hash(state) }
            RawConstantEntry::Float(ref f) => { f.to_bits().hash(state) }
            RawConstantEntry::Long(ref l) => { l.hash(state) }
            RawConstantEntry::Double(ref d) => { d.to_bits().hash(state) }
            RawConstantEntry::Class(ref u) | RawConstantEntry::String(ref u) | RawConstantEntry::MethodType(ref u) | RawConstantEntry::Module(ref u) | RawConstantEntry::Package(ref u) => { u.hash(state) }
            RawConstantEntry::Field(ref u1, ref u2) | RawConstantEntry::Method(ref u1, ref u2) | RawConstantEntry::InterfaceMethod(ref u1, ref u2) | RawConstantEntry::NameAndType(ref u1, ref u2) | RawConstantEntry::Dynamic(ref u1, ref u2) | RawConstantEntry::InvokeDynamic(ref u1, ref u2) => {
                u1.hash(state);
                u2.hash(state);
            }
            RawConstantEntry::MethodHandle(b, u) => {
                b.hash(state);
                u.hash(state);
            }

        }
    }
}