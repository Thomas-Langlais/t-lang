use web_sys::KeyboardEvent;

pub enum Key {
    Backspace,
    Tab,
    Enter,
    Char(char),
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    Eol,
    Eof,
    Interrupt,
    Unknown(String),
}

impl From<KeyboardEvent> for Key {
    fn from(ev: KeyboardEvent) -> Self {
        match ev.key_code() as u8 {
            8 => Self::Backspace,
            9 => Self::Tab,
            
            10 => Self::Eol,
            13 if ev.shift_key() => Self::Eol,
            13 => Self::Enter,

            37 => Self::ArrowLeft,
            38 => Self::ArrowUp,
            39 => Self::ArrowRight,
            40 => Self::ArrowDown,
            
            b'C' if ev.ctrl_key() => Self::Interrupt,
            b'D' if ev.ctrl_key() => Self::Eof,

            _ => {
                let printable = !ev.alt_key() && !ev.meta_key() && !ev.ctrl_key();
                let chars = ev.key().chars().collect::<Vec<char>>();
                if printable && chars.len() == 1 {
                    Self::Char(chars[0])
                } else {
                    Self::Unknown(format!("<keycode={}>", ev.key_code()))
                }
            }
        }
    }
}

impl Key {
    pub fn is_ctrl_key(&self) -> bool {
        matches!(self, Self::Interrupt | Self::Eof)
    }
}