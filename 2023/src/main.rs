use clap::{Parser, Subcommand};
use anyhow::Result;

mod prepare;
mod solutions;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Options {
    #[command(subcommand)]
    command: Command
}

#[derive(Subcommand, Debug)]
enum Command {
    Prepare(prepare::PrepareCommandOptions),
    Run(solutions::RunCommandOptions)
}


fn main() -> Result<()> {
    let options = Options::parse();

    match options.command {
        Command::Prepare(options) => prepare::prepare(options)?,
        Command::Run(options) => solutions::run(options),
    }

    Ok(())
}
