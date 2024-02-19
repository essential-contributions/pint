use clap::Parser;

#[derive(Debug, Default, Parser)]
pub struct Args {
    #[arg(value_parser, required = true)]
    pub filepaths: Vec<String>,
}
