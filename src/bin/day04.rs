use std::num::ParseIntError;
use std::str::FromStr;

use aoc2022::read_input;
use aoc2022::split_into_tuple;

#[derive(Debug)]
struct Assignement {
    lower: u32,
    upper: u32,
}

fn main() {
    let lines = read_input("inputs/day04/input.txt");

    let part_1 = part_1(&lines);
    std::println!("Part 1: {}", part_1);

    let part_2 = part_2(&lines);
    std::println!("Part 2: {}", part_2);
}

fn part_1(input: &[String]) -> u32 {
    let mut count = 0;

    for line in input.iter() {
        let (elf1, elf2) = parse_assignements(line);

        if elf1.contains(&elf2) {
            count += 1;
        }
    }

    count
}

fn part_2(input: &[String]) -> u32 {
    let mut count = 0;

    for line in input.iter() {
        let (elf1, elf2) = parse_assignements(line);

        if elf1.overlaps(&elf2) {
            count += 1;
        }
    }

    count
}

impl FromStr for Assignement {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (lower_bound, upper_bound) = split_into_tuple(s, '-');

        Ok(Self {
            lower: lower_bound,
            upper: upper_bound,
        })
    }
}

impl Assignement {
    fn overlaps(&self, other: &Assignement) -> bool {
        (self.lower <= other.lower && self.upper >= other.lower)
            || (self.lower <= other.upper && self.upper >= other.upper)
            || (other.lower <= self.lower && other.upper >= self.lower)
            || (other.lower <= self.upper && other.upper >= self.upper)
    }

    fn contains(&self, other: &Assignement) -> bool {
        (self.lower <= other.lower && self.upper >= other.upper)
            || (other.lower <= self.lower && other.upper >= self.upper)
    }
}

fn parse_assignements(line: &str) -> (Assignement, Assignement) {
    split_into_tuple(line, ',')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day04/input.test");
        let expected_result = 2;

        let result = part_1(&test_input);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day04/input.test");
        let expected_result = 4;

        let result = part_2(&test_input);

        assert_eq!(expected_result, result);
    }
}
