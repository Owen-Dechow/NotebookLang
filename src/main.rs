use std::path::Path;
mod lexer;
mod location;
mod parser;

fn run_file<T: AsRef<Path>>(file: T) -> Result<(), Box<dyn std::error::Error>> {
    let mut ts = lexer::lex_file(file)?;
    let abstract_st = parser::parse_token_stream(&mut ts);
    // dbg!(abstract_st);

    return Ok(());
}

fn main() {
    let mut args = std::env::args();
    let file = args.nth(1);

    if let Some(file) = file {
        let _ = run_file(file);
    } else {
        println!("No file passed")
    }
}
