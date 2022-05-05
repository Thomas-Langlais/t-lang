use crate::exec::RTError;
use crate::lexer::Source;
use crate::parser::Error;

pub enum CoreError {
    ParserErr(Error),
    RuntimeErr(RTError),
}

impl From<RTError> for CoreError {
    fn from(err: RTError) -> Self {
        CoreError::RuntimeErr(err)
    }
}

impl From<Error> for CoreError {
    fn from(err: Error) -> Self {
        CoreError::ParserErr(err)
    }
}

fn internal_format_err(name: String, details: String, source: Source, text: String) -> Vec<u8> {
    let line_header = format!("line {line}: ", line = source.start.line);

    let underline = (1..source.start.column + line_header.len())
        .map(|_| ' ')
        .chain((source.start.index..=source.end.index).map(|_| '^'))
        .collect::<String>();

    let line = text
        .lines()
        .enumerate()
        .skip_while(|(i, _)| i + 1 != source.start.line)
        .map(|(_, line)| line)
        .next()
        .unwrap();

    format!(
        "{name} - {details}\n\
        {line_header}{line}\n\
        {underline}"
    ).into_bytes()
}

pub fn format_err<E: Into<CoreError>>(err: E, text: String, src: Option<Source>) -> Vec<u8> {
    let default_source = if let Some(source) = src {
        source
    } else {
        Source::default()
    };

    match err.into() {
        CoreError::ParserErr(err) => {
            let (details, source) = match err {
                Error::Bad(msg, source) => (msg.to_string(), source),
                Error::Io(err) => (err.to_string(), default_source),
            };
            internal_format_err("Illegal Syntax Error".to_string(), details, source, text)
        }
        CoreError::RuntimeErr(err) => internal_format_err(
            err.name.to_string(),
            err.details.to_string(),
            default_source,
            text,
        ),
    }
}
