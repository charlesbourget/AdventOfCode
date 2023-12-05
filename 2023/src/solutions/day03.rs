use crate::solutions::Parts;
use anyhow::Result;

use super::utils::read_input_str;

const INPUT: &str = include_str!("../../inputs/day03/input.txt");

pub fn run(parts: Parts) -> Result<()> {
    println!("Day 03");
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

fn part_1(input: &[String]) -> Result<i64> {
    let input_map = parse_input(input);
    let mut response = 0;

    for (i, line) in input_map.iter().enumerate() {
        let mut current_num: Vec<char> = Vec::new();
        let mut counts = false;

        for (j, char) in line.iter().enumerate() {
            if char.is_ascii_digit() {
                // We have a number, add to list and check if it touches a symbol
                current_num.push(*char);
                counts |= touches_symbol(i, j, &input_map);
            } else {
                // We have either a symbol or a dot
                if !current_num.is_empty() && counts {
                    // Create the current number and add it to response
                    let num: String = current_num.iter().collect();
                    response += num.parse::<i64>().unwrap();
                }

                current_num.clear();
                counts = false;
            }
        }
    }

    println!("\tPart 1: {}", response);
    Ok(response)
}

fn touches_symbol(i: usize, j: usize, input_map: &[Vec<char>]) -> bool {
    is_symbol(input_map[i - 1][j])
        || is_symbol(input_map[i + 1][j])
        || is_symbol(input_map[i][j - 1])
        || is_symbol(input_map[i][j + 1])
        || is_symbol(input_map[i - 1][j - 1])
        || is_symbol(input_map[i - 1][j + 1])
        || is_symbol(input_map[i + 1][j - 1])
        || is_symbol(input_map[i + 1][j + 1])
}

fn is_symbol(c: char) -> bool {
    !c.is_ascii_digit() && c != '.'
}

fn part_2(input: &[String]) -> Result<i32> {
    let input_map = parse_input(input);
    let mut response = 0;

    for (i, line) in input_map.iter().enumerate() {
        for (j, char) in line.iter().enumerate() {
            let mut numbers: Vec<i32> = Vec::new();
            if *char == '*' {
                // Check every direction for a number;
                // Once we find one, consume it all

                // Left
                if input_map[i][j - 1].is_ascii_digit() {
                    numbers.push(consume_left(&input_map, i, j));
                }

                // Right
                if input_map[i][j + 1].is_ascii_digit() {
                    numbers.push(consume_right(&input_map, i, j + 1));
                }

                // Up, special case, check in both directions. We are maybe in the middle of a number
                if input_map[i - 1][j].is_ascii_digit() {
                    numbers.push(consume_both_ways(&input_map, i - 1, j))
                } else {
                    // Upper left corner
                    if input_map[i - 1][j - 1].is_ascii_digit() {
                        numbers.push(consume_left(&input_map, i - 1, j));
                    }

                    // Upper right corner
                    if input_map[i - 1][j + 1].is_ascii_digit() {
                        numbers.push(consume_right(&input_map, i - 1, j + 1));
                    }
                }

                // Down, special case, check in both directions. We are maybe in the middle of a number
                if input_map[i + 1][j].is_ascii_digit() {
                    numbers.push(consume_both_ways(&input_map, i + 1, j))
                } else {
                    // Lower left corner
                    if input_map[i + 1][j - 1].is_ascii_digit() {
                        numbers.push(consume_left(&input_map, i + 1, j));
                    }

                    // Lower right corner
                    if input_map[i + 1][j + 1].is_ascii_digit() {
                        numbers.push(consume_right(&input_map, i + 1, j + 1));
                    }
                }

                // Only add if we have 2 numbers
                if numbers.len() == 2 {
                    response += numbers[0] * numbers[1];
                }
            }
        }
    }

    println!("\tPart 2: {}", response);
    Ok(response)
}

fn consume_left(input_map: &[Vec<char>], start_y: usize, start_x: usize) -> i32 {
    let mut current_number: Vec<char> = Vec::new();

    for j in (0..start_x).rev() {
        if input_map[start_y][j].is_ascii_digit() {
            current_number.push(input_map[start_y][j]);
        } else {
            break;
        }
    }

    current_number
        .iter()
        .rev()
        .collect::<String>()
        .parse::<i32>()
        .unwrap()
}

fn consume_right(input_map: &[Vec<char>], start_y: usize, start_x: usize) -> i32 {
    let mut current_number: Vec<char> = Vec::new();

    for j in start_x..input_map[start_y].len() {
        if input_map[start_y][j].is_ascii_digit() {
            current_number.push(input_map[start_y][j]);
        } else {
            break;
        }
    }

    current_number
        .iter()
        .collect::<String>()
        .parse::<i32>()
        .unwrap()
}

fn consume_both_ways(input_map: &[Vec<char>], start_y: usize, start_x: usize) -> i32 {
    let mut current_number: Vec<char> = Vec::new();

    for j in (0..start_x).rev() {
        if input_map[start_y][j].is_ascii_digit() {
            current_number.push(input_map[start_y][j]);
        } else {
            break;
        }
    }

    current_number.reverse();

    for j in start_x..input_map[start_y].len() {
        if input_map[start_y][j].is_ascii_digit() {
            current_number.push(input_map[start_y][j]);
        } else {
            break;
        }
    }

    current_number
        .iter()
        .collect::<String>()
        .parse::<i32>()
        .unwrap()
}

fn parse_input(input: &[String]) -> Vec<Vec<char>> {
    let mut map = Vec::new();
    let lenth_line = input[0].len() + 2;
    map.push(vec!['.'; lenth_line]);

    for line in input {
        let mut vec_line = vec!['.'];
        vec_line.extend(line.chars());
        vec_line.push('.');
        map.push(vec_line);
    }

    map.push(vec!['.'; lenth_line]);

    map
}

#[cfg(test)]
mod tests {
    extern crate test;

    use crate::solutions::utils::read_input;
    use test::Bencher;

    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day03/input.test").unwrap();
        let expected_result = 4361;
        let result = part_1(&test_input).unwrap();
        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day03/input.test").unwrap();
        let expected_result = 467835;
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
