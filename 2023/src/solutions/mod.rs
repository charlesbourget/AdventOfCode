use anyhow::Result;
use clap::{arg, Args, ValueEnum};

#[allow(dead_code)]
mod utils;

const fn day_range() -> std::ops::RangeInclusive<u8> {
    1..=25
}

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
    One,
    Two,
}

pub fn run(options: RunCommandOptions) {
    if options.all {
        for day in day_range() {
            run_specified_day(day);
        }
    } else {
        let day = match options.day {
            Some(day) => day,
            None => {
                eprintln!("Please specify a day to run");
                return;
            }
        };

        run_specified_day(day);
    }

}

fn run_specified_day(day: u8) {
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
