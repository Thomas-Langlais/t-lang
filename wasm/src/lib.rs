extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

mod utils;

use lang::lexer::Lexer;
use lang::parser::Parser;
use lang::interpreter::Execute;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;


#[wasm_bindgen]
pub fn run(source: &str) -> String {
    utils::set_panic_hook();

    let buffer = Vec::from(source.as_bytes());

    // set the io handle for reading the stream
    let mut reader = Lexer::new(&buffer);
    let tokens = reader.parse_tokens();


    let mut parser = Parser::new(tokens);
    let ast_result = parser.generate_syntax_tree();
    
    if let Err(err) = ast_result {
        return format!("{}\n", err);
    }

    let ast = unsafe { ast_result.unwrap_unchecked() };
    
    match ast.execute() {
        Ok(inter_type) => format!("= {}\n", inter_type),
        Err(err) => format!("{}\n", err)
    }
}