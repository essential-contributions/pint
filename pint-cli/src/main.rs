use clap::{builder::styling::Style, Parser};

fn main() {
    let pint = pint_cli::Pint::parse();
    if let Err(err) = pint_cli::run(pint) {
        let bold = Style::new().bold();
        eprintln!("{}Error:{} {err:?}", bold.render(), bold.render_reset());
        std::process::exit(1);
    }
}
