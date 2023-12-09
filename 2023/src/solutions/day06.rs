use super::utils::read_input_str;
use crate::solutions::Parts;
use anyhow::Result;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;

const INPUT: &str = include_str!("../../inputs/day06/input.txt");
const SPEED_INCREASE: u64 = 1; // ms/s

pub fn run(parts: Parts) -> Result<()> {
    println!("Day 06");
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
    let response =  parse_input(input)
        .iter()
        .map(find_optimal_time)
        .fold(1, |a, b| a * b as i32);

    println!("\tPart 1: {}", response);
    Ok(response)
}

fn part_2(input: &[String]) -> Result<u64> {
    let response = find_optimal_time_par(&parse_input_2(input));

    println!("\tPart 2: {}", response);
    Ok(response)
}

struct Race {
    pub time: u64,
    pub distance: u64,
}

fn parse_input(input: &[String]) -> Vec<Race> {
    let mut races: Vec<Race> = Vec::new();

    let time_line: Vec<&str> = input[0].split_ascii_whitespace().skip(1).collect();
    let distance_line: Vec<&str> = input[1].split_ascii_whitespace().skip(1).collect();

    for (i, time) in time_line.iter().enumerate() {
        races.push(Race {
            time: time.parse().unwrap(),
            distance: distance_line[i].parse().unwrap(),
        })
    }

    races
}

fn parse_input_2(input: &[String]) -> Race {
    let time_line: String = input[0].split(':').nth(1).unwrap().replace(' ', "");
    let distance_line: String = input[1].split(':').nth(1).unwrap().replace(' ', "");

    Race {
        time: time_line.parse().unwrap(),
        distance: distance_line.parse().unwrap(),
    }
}

fn find_optimal_time(race: &Race) -> u64 {
    (0..=race.time)
        .map(|i| (SPEED_INCREASE * i) * (race.time - i))
        .filter(|distance| distance > &race.distance)
        .count() as u64
}

/// Parallel implementation for bigger part 2 numbers
fn find_optimal_time_par(race: &Race) -> u64 {
    (0..=race.time)
        .into_par_iter()
        .map(|i| (SPEED_INCREASE * i) * (race.time - i))
        .filter(|distance| distance > &race.distance)
        .count() as u64
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use crate::solutions::utils::read_input;
    use test::Bencher;

    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day06/input.test").unwrap();
        let expected_result = 288;
        let result = part_1(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day06/input.test").unwrap();
        let expected_result = 71503;
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
