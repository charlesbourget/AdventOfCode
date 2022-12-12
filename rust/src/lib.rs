use std::fmt::Debug;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use std::str::FromStr;

mod strings;

pub use crate::strings::operations;

pub fn read_input(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

pub fn read_input_i32(filename: impl AsRef<Path>) -> Vec<i32> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .map(|l| l.parse::<i32>().expect("Could not parse int"))
        .collect()
}

pub fn split_into_tuple<T: FromStr + Debug>(input: &str, separator: char) -> (T, T)
where
    <T as FromStr>::Err: Debug,
{
    strings::operations::split_into_tuple(input, separator)
}
