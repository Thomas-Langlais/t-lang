use crate::exec::{Error as ExecError, RTError};
use crate::lexer::Source;
use crate::parser::Error;

pub enum CoreError {
    ParserErr(Error),
    RuntimeErr(RTError, Source),
    StoppedErr(u8),
}

impl From<Error> for CoreError {
    fn from(err: Error) -> Self {
        CoreError::ParserErr(err)
    }
}

impl From<ExecError> for CoreError {
    fn from(err: ExecError) -> Self {
        match err {
            ExecError::ParserError(err) => CoreError::ParserErr(err),
            ExecError::RuntimeError(err, source) => CoreError::RuntimeErr(err, source),
            ExecError::StoppedError(code) => CoreError::StoppedErr(code),
        }
    }
}

fn internal_format_source_err(
    name: String,
    details: String,
    mut source: Source,
    text: String,
) -> Vec<u8> {
    // handle when the source is on a new line, this is when the column is 0.
    // the column is never 0 on any non-newline characters
    if source.start.column == 0 {
        source.start.line -= 1;
        // set the column
        let mut index = source.start.index;
        let mut column = 1;
        while index > 0 && text.chars().nth(index - 1).unwrap() != '\n' {
            index -= 1;
            column += 1;
        }
        source.start.column = column;
    }

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
    )
    .into_bytes()
}

pub fn format_err<E: Into<CoreError>>(err: E, text: String) -> Vec<u8> {
    match err.into() {
        CoreError::ParserErr(err) => match err {
            Error::Bad(msg, source) => internal_format_source_err(
                "Illegal Syntax Error".to_string(),
                msg.to_string(),
                source,
                text,
            ),
            Error::Io(err) => err.to_string().into_bytes(),
        },
        CoreError::RuntimeErr(err, source) => {
            internal_format_source_err(err.name.to_string(), err.details.to_string(), source, text)
        }
        CoreError::StoppedErr(code) => format!("Exited with code: {}", code).into_bytes(),
    }
}
