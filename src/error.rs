use thiserror::Error;
#[derive(Debug, Error)]
pub enum Error {
    #[error("IO Error")]
    IO(#[from] std::io::Error)
}
pub type Result<T> = std::result::Result<T, Error>;