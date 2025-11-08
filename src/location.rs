use std::{ops::Range, path::PathBuf, usize};

#[derive(Debug, Clone)]
pub(crate) struct Location {
    pub(crate) file: PathBuf,
    pub(crate) file_range: Range<usize>,
}
