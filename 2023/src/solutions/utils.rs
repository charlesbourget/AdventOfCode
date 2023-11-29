use std::{path::Path, fs::File, io::{BufReader, BufRead}};

use anyhow::{Result, Context};

pub fn read_input(filename: impl AsRef<Path>) -> Result<Vec<String>> {
    let file = File::open(filename).context("Unable to open input file")?;
    let buf = BufReader::new(file);
    let lines = buf.lines().collect::<Result<Vec<String>,_>>()?;

    Ok(lines)
}

pub fn read_input_i32(filename: impl AsRef<Path>) -> Result<Vec<i32>> {
    let lines = read_input(filename)?;

    let parsed_lines = lines.iter()
        .map(|line| line.parse::<i32>())
        .collect::<Result<Vec<i32>, _>>()?;

    Ok(parsed_lines)
}