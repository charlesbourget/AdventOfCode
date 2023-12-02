use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

use anyhow::{Context, Result};

pub fn read_input(filename: impl AsRef<Path>) -> Result<Vec<String>> {
    let file = File::open(filename).context("Unable to open input file")?;
    let buf = BufReader::new(file);
    let lines = buf.lines().collect::<Result<Vec<String>, _>>()?;

    Ok(lines)
}

pub fn read_input_i32(filename: impl AsRef<Path>) -> Result<Vec<i32>> {
    let lines = read_input(filename)?;

    let parsed_lines = lines
        .iter()
        .map(|line| line.parse::<i32>())
        .collect::<Result<Vec<i32>, _>>()?;

    Ok(parsed_lines)
}

pub fn split_into_tuple(input: &str, separator: &str) -> (String, String) {
    let mut bounds = input.split(separator);
    let lower_bound = bounds.next().unwrap();
    let upper_bound = bounds.next().unwrap();

    (lower_bound.to_owned(), upper_bound.to_owned())
}
