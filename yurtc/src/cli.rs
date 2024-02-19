use clap::Parser;

#[derive(Debug, Default, Parser)]
pub struct Args {
    #[arg(long = "solve", short = 's')]
    pub solve: bool,

    #[arg(value_parser)]
    pub filepath: String,

    #[arg(long = "output", short = 'o')]
    pub output: Option<String>,
}
