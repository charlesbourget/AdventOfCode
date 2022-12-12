use aoc2022::read_input;
use std::collections::HashMap;

fn main() {
    let lines = read_input("../inputs/day02/input.txt");

    let part_1 = part_1(&lines);
    std::println!("Part 1: {}", part_1);

    let part_2 = part_2(&lines);
    std::println!("Part 2: {}", part_2);
}

fn part_1(input: &[String]) -> u32 {
    let score_map = HashMap::from([
        ("A X", 4),
        ("A Y", 8),
        ("A Z", 3),
        ("B X", 1),
        ("B Y", 5),
        ("B Z", 9),
        ("C X", 7),
        ("C Y", 2),
        ("C Z", 6),
    ]);

    input
        .iter()
        .map(|line| score_map.get(line as &str).unwrap())
        .sum()
}

fn part_2(input: &[String]) -> u32 {
    let score_map = HashMap::from([
        ("A X", 3),
        ("A Y", 4),
        ("A Z", 8),
        ("B X", 1),
        ("B Y", 5),
        ("B Z", 9),
        ("C X", 2),
        ("C Y", 6),
        ("C Z", 7),
    ]);

    input
        .iter()
        .map(|line| score_map.get(line as &str).unwrap())
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = read_input("../inputs/day02/input.test");
        let expected_result = 15;

        let result = part_1(&test_input);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("../inputs/day02/input.test");
        let expected_result = 12;

        let result = part_2(&test_input);

        assert_eq!(expected_result, result);
    }
}
