#[macro_use]
extern crate lazy_static;
use std::fs::File;
use std::io::prelude::*;
use std::env::args;

mod compiler;
mod executor;
mod ast;
mod builtins;

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
    let ast = Ast::from_str(&src).expect("Could not buid ast");
    // Compile into instructions
    let instructions = Compiler::compile(&ast.root).unwrap();
    // execute the instructions
    println!("script compiled into:");
    println!("{:>7} | Instruction", "Addr");
    for (i, inst) in instructions.iter().enumerate() {
        println!("{:7} | {:?}", i, inst);
    }
    let mut executor = Executor::from_instructions(&instructions);
    let value = executor.execute().expect("No value returned");
    println!("\nExecution returned: {:#?}", value);
    Ok(())
}

