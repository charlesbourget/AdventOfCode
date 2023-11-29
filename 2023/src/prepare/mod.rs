use std::{io::Write, fs};

use anyhow::Result;
use clap::Args;

use self::template::generate_template;

mod template;
mod config;

#[derive(Args, Debug)]
pub struct PrepareCommandOptions {
    day: u8,

    /// Do not fetch input
    #[arg(long)]
    disable_fetch: bool,

    /// Do not generate template
    #[arg(long)]
    disable_codegen: bool,
}

pub fn prepare(options: PrepareCommandOptions) -> Result<()> {
    let day = options.day;
    let formatted_day = format!("{:0>2}", day);

    let config = config::load_config()?;

    if !options.disable_fetch {
        let input = fetch_input(day, &config.session)?;
        write_input(&formatted_day, &input)?;
    } else {
        println!("Skipping fetching input for day {}", day);
    }

    if !options.disable_codegen {
        let generated_code = generate_template(&formatted_day)?;
        write_template(&formatted_day, generated_code)?;
    } else {
        println!("Skipping code generation for day {}", day);
    }

    Ok(())
}


fn fetch_input(day: u8, session_token: &str) -> Result<String> {
    let url = format!("https://adventofcode.com/2022/day/{}/input", day);

    let client = reqwest::blocking::Client::new();
    let res = client
        .get(url)
        .header("Cookie", format!("session={}", session_token))
        .send()?;

    let input = res.text()?;

    Ok(input)
}

fn write_input(day: &str, input: &str) -> Result<()> {
    let directory = format!("inputs/day{:0>2}", day);
    fs::create_dir(&directory)?;

    let mut file = fs::File::create(format!("{}/input.txt", &directory))?;
    file.write_all(input.as_bytes())?;

    Ok(())
}

fn write_template(day: &str, code: String) -> Result<()> {
    let path = format!("src/solutions/day{}.rs", day);
    let mut file = fs::File::create(path)?;
    file.write_all(code.as_bytes())?;

    Ok(())
}