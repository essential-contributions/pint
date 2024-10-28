use clap::Parser;

#[derive(Debug, Default, Parser)]
pub struct Args {
    #[arg(value_parser)]
    pub filepath: String,

    #[arg(long = "output", short = 'o')]
    pub output: Option<String>,

    #[arg(long, value_parser = parse_hex_salt)]
    pub salt: Option<[u8; 32]>,

    #[arg(long = "print-parsed")]
    pub print_parsed: bool,

    #[arg(long = "print-flat")]
    pub print_flat: bool,

    #[arg(long = "print-optimized")]
    pub print_optimized: bool,

    #[arg(long = "print-asm")]
    pub print_asm: bool,

    #[arg(long = "skip-optimize", hide = true)]
    pub skip_optimize: bool,
}

/// Parses a `&str` that represents a 256-bit unsigned integer in hexadecimal format and converts
/// it into a `[u8; 32]`.
///
/// Emits an error if the conversion is not possible.
fn parse_hex_salt(value: &str) -> Result<[u8; 32], String> {
    if value.len() == 64 && value.chars().all(|c| c.is_ascii_hexdigit()) {
        let mut salt = [0u8; 32];
        for i in 0..32 {
            salt[i] = u8::from_str_radix(&value[2 * i..2 * i + 2], 16)
                .map_err(|_| "Invalid hexadecimal value")?;
        }
        Ok(salt)
    } else {
        Err("Salt must be a 256-bit hexadecimal string (64 hex characters)".to_string())
    }
}
