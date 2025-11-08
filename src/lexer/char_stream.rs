use std::{path::Path, str::Chars};

use crate::location::Location;

pub(super) struct CharStream<'a> {
    file: &'a Path,
    chars: Chars<'a>,
    location: usize,
    last: Option<char>,
    retake: bool,
}

impl<'a> CharStream<'a> {
    pub(super) fn new<S: AsRef<str>, P: AsRef<Path>>(string: &'a S, file: &'a P) -> Self {
        Self {
            chars: string.as_ref().chars(),
            location: 0,
            retake: false,
            last: None,
            file: file.as_ref(),
        }
    }

    pub(super) fn take(&mut self) -> Option<char> {
        if self.retake {
            self.retake = false;
        } else {
            self.location += 1;
            self.last = self.chars.next();
        }

        return self.last;
    }

    pub(super) fn take_sig(&mut self) -> Option<char> {
        let c = self.take()?;

        if c.is_whitespace() {
            return self.take_sig();
        } else {
            self.retake();
            return Some(c);
        }
    }

    pub(super) fn retake(&mut self) {
        self.retake = true;
    }

    pub(super) fn build_loc<T: AsRef<str>>(&self, value: T) -> crate::location::Location {
        Location {
            file: self.file.to_path_buf(),
            file_range: self.location - 1 - value.as_ref().chars().count()..(self.location - 1),
        }
    }
}
