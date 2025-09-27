use std::{ops::Range, path::PathBuf, usize};


#[derive(Debug)]
pub struct Location {
    pub file: PathBuf,
    pub file_range: Range<usize>,
}
