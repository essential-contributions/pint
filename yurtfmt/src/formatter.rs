pub(super) type FormattedCode = String;

// Basic `Format` trait. We will likely need to pass around a `Formatter` object to `format()` in
// the future to carry over things like formatter configs and utils.
pub(super) trait Format {
    fn format(
        &self,
        formatted_code: &mut FormattedCode,
    ) -> Result<(), crate::error::FormatterError>;
}
