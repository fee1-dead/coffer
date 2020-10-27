use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO Error")]
    IO(#[from] std::io::Error),
    #[error("Extra bytes ({0}) remaining in class file.")]
    ExtraBytes(u64),
    #[error("Reached End Of File")]
    EOF,
    #[error("unrecognized {0}: {1}")]
    Unrecognized(&'static str, String),
    #[error(transparent)]
    MUTF(#[from] crate::mod_utf8::MUTFError)
}
pub type Result<T> = std::result::Result<T, Error>;