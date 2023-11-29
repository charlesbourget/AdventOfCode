use anyhow::Result;
use clap::{arg, Args, ValueEnum};

#[allow(dead_code)]
mod utils;

#[derive(Args, Debug)]
pub struct RunCommandOptions {
    /// Day to run
    #[arg(short, long)]
    day: Option<u8>,

    /// Run all days (will override --day)
    #[arg(short, long)]
    all: bool,

    #[arg(short, long, value_enum, default_value_t = Parts::Both)]
    parts: Parts,
}

#[derive(ValueEnum, Debug, Clone, Copy)]
pub enum Parts {
    Both,
    Part1,
    Part2,
}

pub fn run(options: RunCommandOptions) {
    // TODO: Implement all

    let day = options.day.unwrap();

    let result: Result<()> = match day {
        _ => {
            eprintln!("Day {} not implemented yet", day);

            Ok(())
        }
    };

    if let Err(e) = result {
        eprintln!("Error running day {}: {}", day, e);
    }
}
