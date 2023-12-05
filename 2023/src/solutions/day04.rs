use std::collections::HashSet;

use crate::solutions::utils::read_input;
use crate::solutions::Parts;
use anyhow::Result;

use super::utils::split_into_tuple;

pub fn run(parts: Parts) -> Result<()> {
    let lines = read_input("inputs/day04/input.txt")?;

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
        .map(|line| parse_input(line))
        .map(|x| x.0.intersection(&x.1).count())
        .fold(0, |acc, x| acc + score_from_count(x));

    println!("Part 1: {}", response);
    Ok(response)
}

fn part_2(input: &[String]) -> Result<i32> {
    let games = input
        .iter()
        .map(|line| parse_input(line))
        .collect::<Vec<_>>();

    let mut count = vec![1; games.len()];

    for i in 0..games.len() {
        // Number of winning numbers for this card
        let current_card_count = games[i].0.intersection(&games[i].1).count();

        // Given all the copied cards
        for j in 1..=current_card_count {
            // Add one number to it's future count
            count[i + j] += count[i];
        }
    }

    let response = count.iter().sum();

    println!("Part 2: {}", response);
    Ok(response)
}

fn parse_input(input: &str) -> (HashSet<i32>, HashSet<i32>) {
    let number_input = input.split(": ").last().unwrap();
    let (winning_numbers, numbers) = split_into_tuple(number_input, " | ");

    let winning_numbers = winning_numbers
        .split_whitespace()
        .map(|x| x.parse::<i32>().unwrap())
        .collect();

    let numbers: HashSet<i32> = numbers
        .split_whitespace()
        .map(|x| x.parse::<i32>().unwrap())
        .collect();

    (winning_numbers, numbers)
}

fn score_from_count(count: usize) -> i32 {
    if count > 0 {
        i32::pow(2, (count - 1).try_into().unwrap())
    } else {
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day04/input.test").unwrap();
        let expected_result = 13;
        let result = part_1(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day04/input.test").unwrap();
        let expected_result = 30;
        let result = part_2(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }
}
