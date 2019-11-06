#[macro_use]
extern crate lazy_static;
use std::fs::File;
use std::io::prelude::*;
use std::env::args;

mod lexer;
mod tokens;
mod patterns;
mod parser;
mod ast;
mod instructions;
use ast::Ast;


fn main() -> std::io::Result<()> {
	// read file path
    let args: Vec<String> = args().collect();
    assert!(args.len() > 1, "You must provide a src file");
	// read source file
    let mut file = File::open(&args[1])?;
    let mut src = String::new();
    file.read_to_string(&mut src)?;

	// build and print the ast
    let ast = Ast::from_str(&src);
    dbg!(ast);

    Ok(())
}

