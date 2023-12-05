#![feature(test)]
use std::{
    fs::{self, OpenOptions},
    io::Write,
};

use anyhow::{Context, Result};
use clap::{Args, Parser, Subcommand};
use once_cell::sync::Lazy;
use regex::Regex;

mod prepare;
mod solutions;

static STAR_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"message=\d*").unwrap());
const README_FILE_PATH: &str = "./README.md";

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Options {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Prepare(prepare::PrepareCommandOptions),
    Run(solutions::RunCommandOptions),
    AddStar(AddStarCommandOptions),
}

#[derive(Args, Debug)]
struct AddStarCommandOptions {
    /// The number of stars to add
    number: u8,
}

fn main() -> Result<()> {
    let options = Options::parse();

    match options.command {
        Command::Prepare(options) => prepare::prepare(options)?,
        Command::Run(options) => solutions::run(options),
        Command::AddStar(options) => add_star(options.number)?,
    }

    Ok(())
}

fn add_star(number: u8) -> Result<()> {
    let contents = fs::read_to_string(README_FILE_PATH)?;
    let current_stars = STAR_REGEX
        .find(&contents)
        .context("Failed to find stars")?
        .as_str()
        .split('=')
        .last()
        .context("Failed to parse stars")?
        .parse::<u8>()?;
    let new = STAR_REGEX.replace_all(&contents, format!("message={}", number + current_stars));

    let mut file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .open(README_FILE_PATH)?;

    let _ = file.write(new.as_bytes())?;

    Ok(())
}
