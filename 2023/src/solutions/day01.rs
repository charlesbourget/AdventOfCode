use crate::solutions::utils::read_input;
use crate::solutions::Parts;
use anyhow::Result;
use regex::Regex;

pub fn run(parts: Parts) -> Result<()> {
    let lines = read_input("inputs/day01/input.txt")?;
    match parts {
        Parts::One => {
            part_1(&lines)?;
        }
        Parts::Two => {
            part_2(&lines)?;
        }
        Parts::Both => {
            part_1(&lines)?;
            part_2(&lines)?;
        }
    }
    Ok(())
}

fn part_1(input: &[String]) -> Result<i32> {
    let re = Regex::new(r"[A-Za-z]").unwrap();

    let response = input
        .iter()
        .map(|line| re.replace_all(line, "").to_string())
        .map(|line| get_digits(&line))
        .map(|line| line.parse::<i32>().unwrap())
        .sum();

    println!("Part 1: {}", response);
    Ok(response)
}

fn part_2(input: &[String]) -> Result<i32> {
    let re = Regex::new(r"[A-Za-z]").unwrap();

    let response = input
        .iter()
        // Would've prefered to use regex but regex::Regex does not allow overlapping capture groups
        .map(|line| {
            line.replace("one", "o1e")
                .replace("two", "t2o")
                .replace("three", "t3e")
                .replace("four", "f4r")
                .replace("five", "f5e")
                .replace("six", "s6x")
                .replace("seven", "s7n")
                .replace("eight", "e8t")
                .replace("nine", "n9e")
                .replace("zero", "z0o")
        })
        .map(|line| re.replace_all(&line, "").to_string())
        .map(|line| get_digits(&line))
        .map(|line| line.parse::<i32>().unwrap())
        .sum();

    println!("Part 2: {}", response);
    Ok(response)
}

fn get_digits(line: &str) -> String {
    let bytes = line.as_bytes();

    let first = bytes[0];
    let last = bytes[bytes.len() - 1];

    String::from_utf8(vec![first, last]).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day01/input.test").unwrap();
        let expected_result = 142;
        let result = part_1(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day01/input_2.test").unwrap();
        let expected_result = 281;
        let result = part_2(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }
}
