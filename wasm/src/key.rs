use web_sys::KeyboardEvent;

#[derive(Clone, Copy)]
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
    Unknown(u32),
}

impl From<KeyboardEvent> for Key {
    fn from(ev: KeyboardEvent) -> Self {
        let any_special_key = ev.alt_key() | ev.ctrl_key() | ev.meta_key() | ev.shift_key();

        match ev.key_code() as u8 {
            8 if !any_special_key => Self::Backspace,
            9 if !any_special_key => Self::Tab,
            
            10 => Self::Eol,
            13 if ev.shift_key() => Self::Eol,
            13 => Self::Enter,

            37 if !any_special_key => Self::ArrowLeft,
            38 if !any_special_key => Self::ArrowUp,
            39 if !any_special_key => Self::ArrowRight,
            40 if !any_special_key => Self::ArrowDown,
            
            b'C' if ev.ctrl_key() => Self::Interrupt,
            b'D' if ev.ctrl_key() => Self::Eof,

            _ => {
                let printable = !ev.alt_key() && !ev.meta_key() && !ev.ctrl_key();
                let chars = ev.key().chars().collect::<Vec<char>>();
                if printable && chars.len() == 1 {
                    Self::Char(chars[0])
                } else {
                    Self::Unknown(ev.key_code())
                }
            }
        }
    }
}