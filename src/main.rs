use std::path::Path;

use crate::lexer::token_stream::TokenStream;

mod lexer;
mod location;

fn run_file<T: AsRef<Path>>(file: T) {
    let ts = TokenStream::from_file(&file);
    let _ = dbg!(ts);
}

fn main() {
    let mut args = std::env::args();
    let file = args.nth(1);

    if let Some(file) = file {
        run_file(file);
    } else {
        println!("No file passed")
    }
}
