// pub(super) type FormattedCode = String;

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
    code: String,
    indent_level: usize,
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

    pub fn indent(&mut self) {
        const INDENT_SIZE: usize = 4; // 4 spaces for indentation.
        let spaces = " ".repeat(self.indent_level * INDENT_SIZE);
        self.code.push_str(&spaces);
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
        if self.code.ends_with('\n') || self.code.is_empty() {
            self.code.push_str(&self.get_indentation());
        }
        self.code.push_str(s);
    }

    pub fn write_line(&mut self, s: &str) {
        self.write(s);
        self.code.push('\n');
    }

    fn get_indentation(&self) -> String {
        const INDENT_SIZE: usize = 4; // 4 spaces for indentation.
        " ".repeat(self.indent_level * INDENT_SIZE)
    }
}
