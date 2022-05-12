extern crate wasm_bindgen;

use async_channel::{Receiver, Sender, TryRecvError};
use std::cell::RefCell;
use std::collections::VecDeque;
use wasm_bindgen::prelude::*;
use web_sys::Element;

use lang::errors::*;
use lang::exec::Machine;

mod input;
mod key;
mod utils;

use crate::input::*;
use crate::key::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_char(s: char);
}

#[derive(Debug, Default, Clone, Copy)]
struct CursorPosition {
    x: usize,
    y: usize,
}

#[wasm_bindgen]
pub struct TerminalApi {
    code_element: Element,
    cur_pos: CursorPosition,
    buffer: VecDeque<VecDeque<char>>,
    input: WebInputApi,
    key_sender: BrowserKeyboardInterface,
    output_sender: Sender<Output>,
    output_receiver: Receiver<Output>,
}

#[wasm_bindgen]
pub struct Output {
    code: String,
    result: String,
}

#[wasm_bindgen]
impl Output {
    #[wasm_bindgen(getter)]
    pub fn code(&self) -> String {
        self.code.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn result(&self) -> String {
        self.result.clone()
    }
}

#[wasm_bindgen]
pub struct OutputInterface {
    inner: Receiver<Output>,
}

#[wasm_bindgen]
impl OutputInterface {
    pub fn poll(&self) -> Option<Output> {
        match self.inner.try_recv() {
            Ok(result) => Some(result),
            Err(TryRecvError::Empty) => None,
            Err(TryRecvError::Closed) => panic!("the output interface should never be closed."),
        }
    }
}

#[wasm_bindgen]
#[allow(clippy::new_without_default)]
impl TerminalApi {
    #[wasm_bindgen(constructor)]
    pub fn new(code_element: Element) -> Self {
        let input = WebInputApi::default();
        let key_sender = input.interface();
        let (output_sender, output_receiver) = async_channel::unbounded();
        let mut buffer = VecDeque::new();
        buffer.push_front(VecDeque::new());

        Self {
            code_element,
            cur_pos: CursorPosition::default(),
            buffer,
            input,
            key_sender,
            output_sender,
            output_receiver,
        }
    }

    pub fn browser_keyboard_interface(&self) -> BrowserKeyboardInterface {
        self.key_sender.clone()
    }

    pub fn output_interface(&self) -> OutputInterface {
        OutputInterface {
            inner: self.output_receiver.clone(),
        }
    }

    pub async fn run_repl_loop(mut self) -> Result<(), i32> {
        let machine_cell = RefCell::new(Machine::new());
        loop {
            let key = self.input.receive_key().await;
            match &key {
                Key::ArrowDown => {
                    if self.cur_pos.y + 1 < self.buffer.len() {
                        self.cur_pos.y += 1;
                    }
                    if self.cur_pos.x > self.buffer[self.cur_pos.y].len() {
                        self.cur_pos.x = self.buffer[self.cur_pos.y].len() - 1;
                    }
                }
                Key::ArrowUp => {
                    if self.cur_pos.y > 0 {
                        self.cur_pos.y -= 1;
                    }
                    if self.cur_pos.x > self.buffer[self.cur_pos.y].len() {
                        self.cur_pos.x = self.buffer[self.cur_pos.y].len() - 1;
                    }
                }
                Key::ArrowLeft => {
                    if self.cur_pos.x > 0 {
                        self.cur_pos.x -= 1;
                    }
                }
                Key::ArrowRight => {
                    if self.cur_pos.x < self.buffer[self.cur_pos.y].len() {
                        self.cur_pos.x += 1;
                    }
                }
                Key::Backspace => {
                    if self.cur_pos.x == 0 && self.cur_pos.y > 0 {
                        let taken_line = self.buffer.remove(self.cur_pos.y).unwrap();
                        let new_x = self.buffer[self.cur_pos.y - 1].len();
                        self.buffer[self.cur_pos.y - 1].extend(taken_line);
                        self.cur_pos.y -= 1;
                        self.cur_pos.x = new_x;
                    } else if self.cur_pos.x > 0 {
                        let line = self.buffer.get_mut(self.cur_pos.y).unwrap();
                        self.cur_pos.x -= 1;
                        line.remove(self.cur_pos.x);
                    }
                }
                &Key::Char(c) => {
                    let line = self.buffer.get_mut(self.cur_pos.y).unwrap();
                    if self.cur_pos.x == 0 {
                        line.push_front(c);
                    } else if self.cur_pos.x == line.len() {
                        line.push_back(c);
                    } else {
                        line.insert(self.cur_pos.x, c);
                    }
                    self.cur_pos.x += 1;
                }
                Key::Eof => log("ended file"),
                Key::Eol => {
                    let taken_line = {
                        let line = self.buffer.get_mut(self.cur_pos.y).unwrap();
                        line.drain(self.cur_pos.x..).collect::<VecDeque<char>>()
                    };

                    if self.cur_pos.y == self.buffer.len() - 1 {
                        self.buffer.push_back(taken_line);
                    } else {
                        self.buffer.insert(self.cur_pos.y, taken_line);
                    }

                    self.cur_pos.y += 1;
                    self.cur_pos.x = 0;
                }
                Key::Interrupt => log("interrupted"),
                Key::Tab => {
                    let line = self.buffer.get_mut(self.cur_pos.y).unwrap();
                    if self.cur_pos.x == 0 {
                        line.push_front('\t')
                    } else if self.cur_pos.x == line.len() {
                        line.push_back('\t')
                    } else {
                        line.insert(self.cur_pos.x, '\t');
                    }
                    self.cur_pos.x += 1;
                }
                Key::Unknown(_) => {}
                Key::Enter => {
                    let source_text = self.buffer.iter().fold(String::new(), |mut acc, line| {
                        acc.extend(line.iter());
                        acc.extend(&['\n']);
                        acc
                    });

                    // reset the buffer to the default state
                    self.buffer.clear();
                    self.buffer.push_front(VecDeque::new());
                    self.cur_pos = CursorPosition::default();

                    let source_text = source_text;
                    let mut bytes = source_text.as_bytes();
                    let mut machine = machine_cell.borrow_mut();

                    let output = match machine.exec(&mut bytes).await {
                        Ok(Some(output)) => format!("{}", output).into(),
                        Err(err) => format_err(err, source_text.clone())
                            .into_iter()
                            .map(char::from)
                            .collect::<String>()
                            .into(),
                        _ => None,
                    };
                    if let Some(msg) = output {
                        if let Err(err) = self
                            .output_sender
                            .send(Output {
                                code: source_text,
                                result: msg,
                            })
                            .await
                        {
                            panic!("Send to unbounded channel should never fail: {}", err);
                        }
                    }
                }
            };

            match &key {
                Key::ArrowDown
                | Key::ArrowLeft
                | Key::ArrowRight
                | Key::ArrowUp
                | Key::Backspace
                | Key::Char(_)
                | Key::Eol
                | Key::Tab
                | Key::Enter => self.draw(),
                _ => {}
            }
        }
    }

    // draw the source text in the code element
    fn draw(&self) {
        fn cursor() -> &'static str {
            "<span class=\"terminal__cursor\"></span>"
        }
        fn text_wrap<'a, I: Iterator<Item = &'a char>>(text: I) -> Vec<char> {
            let mut result = Vec::from_iter("<span class=\"terminal__text\">".chars());
            result.extend(text);
            result.extend("</span>".chars());
            result
        }
        fn draw_active_line(line: &VecDeque<char>, column: usize) -> Vec<char> {
            let mut code_line = Vec::with_capacity(line.len());
            if column == line.len() {
                code_line.extend(text_wrap(line.iter()));
                code_line.extend(cursor().chars());
                return code_line;
            }
            code_line.extend(text_wrap(line.range(0..column)));
            code_line.extend(cursor().chars());
            code_line.extend(text_wrap(line.range(column..)));

            code_line
        }

        let position = self.cur_pos;
        let code_html = self
            .buffer
            .iter()
            .enumerate()
            .fold(String::new(), |mut acc, (i, line)| {
                if i == position.y {
                    acc.extend(draw_active_line(line, position.x));
                } else {
                    acc.extend(text_wrap(line.iter()));
                }
                acc.extend(&['\n']);
                acc
            });
        self.code_element.set_inner_html(&code_html)
    }
}

// this doesn't get called at the start... report the bug
// #[wasm_bindgen(start)]
#[wasm_bindgen]
pub fn start() {
    log("starting...");
    utils::set_panic_hook();
}
