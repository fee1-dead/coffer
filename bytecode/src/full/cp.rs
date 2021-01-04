#[derive(ReadWrite)]
#[tag_type(u8)]
pub enum RawConstantEntry {
    #[tag(1)]
    UTF8(String),
    #[tag(3)]
    Int(i32),
    Float(f32),
    Long
}