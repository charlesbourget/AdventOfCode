use super::utils::read_input_str;
use crate::solutions::Parts;
use anyhow::Result;

const INPUT: &str = include_str!("../../inputs/day09/input.txt");

pub fn run(parts: Parts) -> Result<()> {
    println!("Day 09");
    let lines = read_input_str(INPUT);

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
    let response = parse_input(input)
        .iter()
        .map(|v| v.last().unwrap() + find_next_value(v))
        .sum();

    println!("\tPart 1: {}", response);
    Ok(response)
}

fn part_2(input: &[String]) -> Result<i32> {
    let response = parse_input(input)
        .iter_mut()
        .map(|v| {
            v.reverse();
            v
        })
        .map(|v| v.last().unwrap() + find_next_value(v))
        .sum();

    println!("\tPart 2: {}", response);
    Ok(response)
}

fn parse_input(input: &[String]) -> Vec<Vec<i32>> {
    input
        .iter()
        .map(|l| {
            l.split_ascii_whitespace()
                .map(|c| c.parse::<i32>().unwrap())
                .collect()
        })
        .collect()
}

fn find_next_value(values: &[i32]) -> i32 {
    let result = values
        .iter()
        .zip(values.iter().skip(1))
        .map(|(a, b)| b - a)
        .collect::<Vec<_>>();

    if only_zeros(&result) {
        return 0;
    }

    result.last().unwrap() + find_next_value(&result)
}

fn only_zeros(values: &[i32]) -> bool {
    values.iter().all(|v| *v == 0)
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use crate::solutions::utils::read_input;
    use test::Bencher;

    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day09/input.test").unwrap();
        let expected_result = 114;
        let result = part_1(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day09/input.test").unwrap();
        let expected_result = 2;
        let result = part_2(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }

    #[bench]
    fn part_1_bench(b: &mut Bencher) {
        let lines = read_input_str(INPUT);
        b.iter(|| part_1(&lines));
    }

    #[bench]
    fn part_2_bench(b: &mut Bencher) {
        let lines = read_input_str(INPUT);
        b.iter(|| part_2(&lines));
    }
}
