use std::collections::HashMap;

use crate::{exec::RTError, ast::{Value, FunctionDeclarationNode}};

#[derive(Debug, Clone)]
pub enum Symbol {
    Value(Value),
    Function(FunctionDeclarationNode),
}

#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub value: Symbol,
    pub is_constant: bool,
}

#[derive(Default, Debug, Clone)]
pub struct SymbolTable {
    symbols: HashMap<String, SymbolEntry>,
}

unsafe impl Sync for SymbolTable {}

impl SymbolTable {
    pub fn new(symbols: HashMap<String, SymbolEntry>) -> Self {
        SymbolTable {
            symbols,
        }
    }

    pub fn get(&self, identifier: &str) -> Option<&Symbol> {
        if let Some(value) = self.symbols.get(identifier) {
            Some(&value.value)
        } else {
            None
        }
    }

    pub fn set(&mut self, identifier: &str, value: Symbol) -> Result<(), RTError> {        
        if let Some(symbol) = self.symbols.get_mut(identifier) {
            if symbol.is_constant {
                return Err(RTError {
                    name: "Constant variable write",
                    details: "Cannot overwrite constant variables: e.g. 'true' and `false`",
                });
            }
            symbol.value = value;
        } else {
            self.symbols.insert(
                identifier.to_string(),
                SymbolEntry {
                    value,
                    is_constant: false
                },
            );
        }
        
        Ok(())
    }

    pub fn remove(&mut self, identifier: &str) {
        self.symbols.remove(identifier);
    }
}