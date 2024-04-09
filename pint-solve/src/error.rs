use thiserror::Error;

#[derive(Error, Debug)]
pub enum SolveError {
    #[error("solver internal error: {msg}")]
    Internal { msg: &'static str },
}
