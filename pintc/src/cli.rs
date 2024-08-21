use clap::Parser;

#[derive(Debug, Default, Parser)]
pub struct Args {
    #[arg(value_parser)]
    pub filepath: String,

    #[arg(long = "output", short = 'o')]
    pub output: Option<String>,

    #[arg(long = "print-parsed")]
    pub print_parsed: bool,

    #[arg(long = "print-flat")]
    pub print_flat: bool,

    #[arg(long = "print-asm")]
    pub print_asm: bool,

    #[arg(long = "print-optimised")]
    pub print_optimised: bool,

    #[arg(long = "skip-optimise")]
    pub skip_optimise: bool,
}
