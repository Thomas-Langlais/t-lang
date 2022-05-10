extern crate wasm_bindgen;

use async_channel::{Receiver, Sender};
use web_sys::KeyboardEvent;
use wasm_bindgen::prelude::*;

use crate::key::Key;

pub struct WebInputApi {
    sender: Sender<Key>,
    receiver: Receiver<Key>,
}

impl Default for WebInputApi {
    fn default() -> Self {
        let (sender, receiver) = async_channel::unbounded();
        Self { sender, receiver }
    }
}

impl WebInputApi {
    pub fn interface(&self) -> BrowserKeyboardInterface {
        let sender = self.sender.clone();
        BrowserKeyboardInterface { sender }
    }

    pub (crate) async fn receive_key(&self) -> Key {
        self.receiver.recv().await.unwrap()
    }
}


#[wasm_bindgen]
#[derive(Clone)]
pub struct BrowserKeyboardInterface {
    sender: Sender<Key>,
}

#[wasm_bindgen]
impl BrowserKeyboardInterface {
    fn send_event<E: Into<Key>>(&self, e: E) {
        if let Err(err) = self.sender.try_send(e.into()) {
            panic!("Send to unbounded channel must succeed: {}", err)
        }
    }

    pub fn send_keyboard_event(&self, ev: KeyboardEvent) {
        self.send_event(ev)
    }
}