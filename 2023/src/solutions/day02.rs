use crate::solutions::utils::{read_input, split_into_tuple};
use crate::solutions::Parts;
use anyhow::Result;

const MAX_RED: u8 = 12;
const MAX_GREEN: u8 = 13;
const MAX_BLUE: u8 = 14;

pub fn run(parts: Parts) -> Result<()> {
    let lines = read_input("inputs/day02/input.txt")?;
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
    let response = input.iter().map(|line| parse_line(line)).sum();
    println!("Part 1: {}", response);
    Ok(response)
}

fn part_2(input: &[String]) -> Result<i32> {
    let response = input.iter().map(|line| parse_line_2(line)).sum();
    println!("Part 2: {}", response);
    Ok(response)
}

fn parse_line(line: &str) -> i32 {
    let (game, rest) = split_into_tuple(line, ": ");
    let index = split_into_tuple(&game, " ").1;

    let result = rest.split("; ").map(is_valid_set).all(|all| all);

    if result {
        index.parse().unwrap()
    } else {
        0
    }
}

fn is_valid_set(set: &str) -> bool {
    let mut red = 0;
    let mut green = 0;
    let mut blue = 0;

    set.split(", ").for_each(|subset| {
        let (number, color) = split_into_tuple(subset, " ");

        match color.as_str() {
            "red" => red += number.parse::<u8>().unwrap(),
            "green" => green += number.parse::<u8>().unwrap(),
            "blue" => blue += number.parse::<u8>().unwrap(),
            _ => panic!("Unexpected color"),
        }
    });

    red <= MAX_RED && green <= MAX_GREEN && blue <= MAX_BLUE
}

fn parse_line_2(line: &str) -> i32 {
    let mut max_red = 0;
    let mut max_green = 0;
    let mut max_blue = 0;

    split_into_tuple(line, ": ")
        .1
        .split("; ")
        .map(count_color)
        .for_each(|(red, green, blue)| {
            max_red = max_red.max(red);
            max_green = max_green.max(green);
            max_blue = max_blue.max(blue);
        });

    max_red * max_green * max_blue
}

fn count_color(set: &str) -> (i32, i32, i32) {
    let mut red = 0;
    let mut green = 0;
    let mut blue = 0;

    set.split(", ").for_each(|subset| {
        let (number, color) = split_into_tuple(subset, " ");

        match color.as_str() {
            "red" => red += number.parse::<i32>().unwrap(),
            "green" => green += number.parse::<i32>().unwrap(),
            "blue" => blue += number.parse::<i32>().unwrap(),
            _ => panic!("Unexpected color"),
        }
    });

    (red, green, blue)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day02/input.test").unwrap();
        let expected_result = 8;
        let result = part_1(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }
    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day02/input.test").unwrap();
        let expected_result = 2286;
        let result = part_2(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }
}
