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
mod preprocessors;
mod instructions;
mod compiler;
mod executor;
use ast::Ast;
use compiler::Compiler;
use executor::Executor;


fn main() -> std::io::Result<()> {
	// read file path
    let args: Vec<String> = args().collect();
    assert!(args.len() > 1, "You must provide a src file");
	// read source file
    let mut file = File::open(&args[1])?;
    let mut src = String::new();
    file.read_to_string(&mut src)?;

	// build the ast
    let ast = Ast::from_str(&src).unwrap();
    // Compile into instructions
    let instructions = Compiler::compile(&ast.root).unwrap();
    // execute the instructions
    for (i, inst) in instructions.iter().enumerate() {
        println!("{:5} {:?}", i, inst);
    }
    let mut executor = Executor::from_instructions(&instructions);
    dbg!(executor.execute());
    Ok(())
}

