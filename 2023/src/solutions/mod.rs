use anyhow::Result;
use clap::{arg, Args, ValueEnum};

mod day01;
mod day02;
mod day03;
mod day04;
mod day06;
mod day09;
#[allow(dead_code)]
mod utils;

static DAYS: &[u8] = &[1, 2, 3, 4, 6, 9];

#[derive(Args, Debug)]
pub struct RunCommandOptions {
    /// Day to run
    #[arg(short, long)]
    day: Option<u8>,

    /// Run all days (will override --day)
    #[arg(short, long)]
    all: bool,

    /// Which parts to run
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
        for day in DAYS {
            run_specified_day(*day, options.parts);
        }
    } else {
        let day = match options.day {
            Some(day) => day,
            None => {
                eprintln!("Please specify a day to run");
                return;
            }
        };

        run_specified_day(day, options.parts);
    }
}

fn run_specified_day(day: u8, parts: Parts) {
    let result: Result<()> = match day {
        1 => day01::run(parts),
        2 => day02::run(parts),
        3 => day03::run(parts),
        4 => day04::run(parts),
        6 => day06::run(parts),
        9 => day09::run(parts),
        _ => {
            eprintln!("Day {} not implemented yet", day);

            Ok(())
        }
    };

    if let Err(e) = result {
        eprintln!("Error running day {}: {}", day, e);
    }
}
