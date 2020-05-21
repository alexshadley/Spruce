extern crate pest;
use pest::Position;

use crate::parser::{Span};

// TODO: fix this awful hack
const PRELUDE_LENGTH: usize = 4;

#[derive(Debug)]
pub struct NameErr {
    pub message: String,
    pub span: Span
}

impl NameErr {
    pub fn as_str(&self, file: &String) -> String{
        let pos = pest::Position::new(file, self.span.start).expect("Failed to find position in error");
        let (line, col) = pos.line_col();
        let true_line = line - PRELUDE_LENGTH;
        let line_text = pos.line_of();

        let mut output = format!("ERROR: {}\n\n", self.message);
        output = format!("{}{}| {}", output, true_line, line_text);

        let line_num_len = format!("{}",true_line).len();
        let spaces = std::iter::repeat(" ").take(line_num_len + 1 + col).collect::<String>();
        output = format!("{}{}^\n\n", output, spaces);

        output
    }
}

#[derive(Debug)]
pub struct TypeErr {
    pub message: String,
    pub span: Span
}

impl TypeErr {
    pub fn as_str(&self, file: &String) -> String{
        let pos = pest::Position::new(file, self.span.start).expect("Failed to find position in error");
        let (line, col) = pos.line_col();
        let true_line = line - PRELUDE_LENGTH;
        let line_text = pos.line_of();

        let mut output = format!("ERROR: {}\n\n", self.message);
        output = format!("{}{}| {}", output, true_line, line_text);

        let line_num_len = format!("{}",true_line).len();
        let spaces = std::iter::repeat(" ").take(line_num_len + 1 + col).collect::<String>();
        output = format!("{}{}^\n\n", output, spaces);

        output
    }
}