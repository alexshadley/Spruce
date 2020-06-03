extern crate pest;

use crate::parser::{NodeInfo};

#[derive(Debug)]
pub struct SpruceErr {
    pub message: String,
    pub info: NodeInfo
}

impl SpruceErr {
    pub fn as_str(&self, files: &Vec<(&str, String)>) -> String {
        let (file, _file_name) = files.iter().filter(|(_, file_name)| {*file_name == self.info.file}).next().expect(format!("could not find file while reporting error: {}", self.info.file).as_str());

        let pos = pest::Position::new(file, self.info.span.start).expect("Failed to find position in error");
        let (line, col) = pos.line_col();
        let line_text = pos.line_of();

        let mut output = format!("Error in {}: {}\n\n", self.info.file, self.message);
        output = format!("{}{}| {}", output, line, line_text);

        let line_num_len = format!("{}", line).len();
        let spaces = std::iter::repeat(" ").take(line_num_len + 1 + col).collect::<String>();
        output = format!("{}{}^\n\n", output, spaces);

        output
    }
}
