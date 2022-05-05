use std::cell::RefCell;
use std::collections::HashMap;

use crate::exec::{ExecutionContext, RTError};

#[derive(Debug, Clone, Copy)]
pub enum SymbolValue {
    Int(i64),
    Float(f64),
    Bool(bool)
}

#[derive(Debug, Clone, Copy)]
pub struct SymbolEntry {
    pub value: SymbolValue,
    pub is_constant: bool,
}

pub struct SymbolTable<'a> {
    pub symbols: RefCell<HashMap<String, SymbolEntry>>,
    parent_context: Option<&'a ExecutionContext<'a>>,
}

unsafe impl<'a> Sync for SymbolTable<'a> {}

impl<'a> SymbolTable<'a> {
    pub fn new(symbols: HashMap<String, SymbolEntry>) -> Self {
        SymbolTable {
            symbols: RefCell::new(symbols),
            parent_context: None,
        }
    }

    pub fn get(&self, identifier: &str) -> Option<SymbolValue> {
        let symbols = self.symbols.borrow();
        if let Some(value) = symbols.get(identifier) {
            return Some(value.value);
        }

        let mut parent_context = &self.parent_context;
        while let Some(parent) = parent_context {
            if let Some(value) = parent.symbol_table.symbols.borrow().get(identifier) {
                return Some(value.value);
            }
            parent_context = &parent.parent_context;
        }

        None
    }

    pub fn set(&self, identifier: &str, value: SymbolValue) -> Result<(), RTError> {
        let mut symbols = self.symbols.borrow_mut();
        
        let symbol_entry = match symbols.get(identifier) {
            Some(symbol) => {
                if symbol.is_constant {
                    return Err(RTError {
                        name: "Constant variable write",
                        details: "Cannot overwrite constant variables: e.g. 'true' and `false`",
                    });
                }
                *symbol
            },
            None => SymbolEntry {
                value,
                is_constant: false
            }
        };

        symbols.insert(
            identifier.to_string(),
            SymbolEntry {
                value,
                ..symbol_entry
            },
        );
        Ok(())
    }

    pub fn remove(&self, identifier: &str) {
        self.symbols.borrow_mut().remove(identifier);
    }
}