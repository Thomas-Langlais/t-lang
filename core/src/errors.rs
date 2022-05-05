use crate::lexer::Source;

pub fn format_err(name: String, details: String, source: Source, text: String) -> String {
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
}
