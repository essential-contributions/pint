// Basic `Format` trait. We will likely need to pass around a `Formatter` object to `format()` in
// the future to carry over things like formatter configs and utils.
pub(super) trait Format {
    fn format(
        &self,
        formatted_code: &mut FormattedCode,
    ) -> Result<(), crate::error::FormatterError>;
}

impl std::fmt::Write for FormattedCode {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.push_str(s);
        Ok(())
    }
}

#[derive(Debug)]
pub(super) struct FormattedCode {
    pub code: String,
    pub indent_level: usize,
}

impl FormattedCode {
    pub fn new() -> Self {
        FormattedCode {
            code: String::new(),
            indent_level: 0,
        }
    }

    pub fn as_str(&self) -> &str {
        &self.code
    }

    pub fn push_str(&mut self, s: &str) {
        self.code.push_str(s);
    }

    pub fn increase_indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn decrease_indent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    pub fn write(&mut self, s: &str) {
        println!(
            "Before write: '{}', Indent Level: {}",
            self.code, self.indent_level
        );
        if self.code.ends_with('\n') || self.code.is_empty() {
            let indentation = self.get_indentation();
            println!("Applying indentation: '{}'", indentation);
            self.code.push_str(&indentation);
        }
        self.code.push_str(s);
        println!("After write: '{}'", self.code);
    }

    pub fn write_line(&mut self, s: &str) {
        println!("Before write_line: '{}'", s);
        self.write(s);
        self.code.push('\n');
        println!("After write_line: '{}'", self.code);
    }

    fn get_indentation(&self) -> String {
        const SPACES_PER_INDENT: usize = 4;
        let indentation = " ".repeat(self.indent_level * SPACES_PER_INDENT);
        println!(
            "Generated indentation: '{}', Indent Level: {}",
            indentation, self.indent_level
        );
        indentation
    }
}
