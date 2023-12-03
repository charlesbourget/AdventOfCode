use crate::solutions::utils::{read_input, split_into_tuple};
use crate::solutions::Parts;
use anyhow::Result;

const MAX_RED: u8 = 12;
const MAX_GREEN: u8 = 13;
const MAX_BLUE: u8 = 14;

struct Game {
    index: i32,
    sets: Vec<Set>,
}

struct Set {
    red: u8,
    green: u8,
    blue: u8,
    valid: bool,
}

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
    let response = input
        .iter()
        .map(|line| Game::new(line))
        .filter(|game| game.sets.iter().all(|set| set.valid))
        .map(|game| game.index)
        .sum();

    println!("Part 1: {}", response);
    Ok(response)
}

fn part_2(input: &[String]) -> Result<i32> {
    let response = input
        .iter()
        .map(|line| Game::new(line))
        .map(|game| game.find_min_color())
        .sum();

    println!("Part 2: {}", response);
    Ok(response)
}

impl Game {
    fn new(line: &str) -> Self {
        let (game, rest) = split_into_tuple(line, ": ");
        let index = split_into_tuple(&game, " ").1;

        let sets = rest.split("; ").map(Set::new).collect();

        Self {
            index: index.parse().unwrap(),
            sets,
        }
    }

    fn find_min_color(&self) -> i32 {
        let mut max_red: i32 = 0;
        let mut max_green: i32 = 0;
        let mut max_blue: i32 = 0;

        self.sets.iter().for_each(|set| {
            max_red = max_red.max(set.red.into());
            max_green = max_green.max(set.green.into());
            max_blue = max_blue.max(set.blue.into());
        });

        max_red * max_green * max_blue
    }
}

impl Set {
    fn new(line: &str) -> Self {
        let mut red = 0;
        let mut green = 0;
        let mut blue = 0;

        line.split(", ").for_each(|subset| {
            let (number, color) = split_into_tuple(subset, " ");

            match color.as_str() {
                "red" => red += number.parse::<u8>().unwrap(),
                "green" => green += number.parse::<u8>().unwrap(),
                "blue" => blue += number.parse::<u8>().unwrap(),
                _ => panic!("Unexpected color"),
            }
        });

        let valid = red <= MAX_RED && green <= MAX_GREEN && blue <= MAX_BLUE;

        Self {
            red,
            green,
            blue,
            valid,
        }
    }
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
