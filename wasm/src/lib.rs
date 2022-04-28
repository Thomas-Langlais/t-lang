extern crate wasm_bindgen;
#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use wasm_bindgen::prelude::*;

mod utils;

use lang::interpreter::{ExecutionContext, Interpret, SymbolTable};
use lang::lexer::Lexer;
use lang::parser::Parser;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

lazy_static! {
    static ref GLOBAL_SYMBOL_TABLE: SymbolTable<'static> = SymbolTable::new(HashMap::new());
}

#[wasm_bindgen]
pub fn run(source: &str) -> String {
    utils::set_panic_hook();

    let bytes = source.as_bytes();
    let buffer = Vec::from(source);

    // set the io handle for reading the stream
    let mut reader = Lexer::new(&buffer);
    let tokens_result = reader.parse_tokens();

    if let Err(err) = tokens_result {
        return format!("{}\n", err);
    }
    let tokens = unsafe { tokens_result.unwrap_unchecked() };

    let mut parser = Parser::new(tokens, bytes);
    let ast_result = parser.generate_syntax_tree();

    if let Err(err) = ast_result {
        return format!("{}\n", err);
    }

    let ast = unsafe { ast_result.unwrap_unchecked() };
    let mut context = ExecutionContext::new(
        String::from_utf8(buffer.to_vec()).unwrap(),
        &GLOBAL_SYMBOL_TABLE,
    );
    match ast.interpret(&mut context) {
        Ok(inter_type) => format!("= {}\n", inter_type),
        Err(err) => format!("{}\n", err),
    }
}
