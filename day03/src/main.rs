use std::collections::HashSet;

fn main() {
    let lines = lib::read_input("input.txt");

    let part_1 = part_1(&lines);
    std::println!("Part 1: {}", part_1);

    let part_2 = part_2(&lines);
    std::println!("Part 2: {}", part_2);
}

fn part_1(input: &[String]) -> i32 {
    input
        .iter()
        .map(|line| -> i32 {
            let first: HashSet<char> = line[..line.len() / 2].to_string().chars().collect();
            let second = line[line.len() / 2..].to_string();

            for c in second.chars() {
                if first.contains(&c) {
                    return to_code_point(c);
                }
            }

            0
        })
        .sum()
}

fn part_2(input: &[String]) -> i32 {
    let mut result: i32 = 0;

    for i in (0..input.len() - 2).step_by(3) {
        let first: HashSet<char> = input[i].to_string().chars().collect();
        let second: HashSet<char> = input[i + 1].to_string().chars().collect();
        let third = input[i + 2].to_string();

        for c in third.chars() {
            if first.contains(&c) && second.contains(&c) {
                result += to_code_point(c);
                break;
            }
        }
    }

    result
}

fn to_code_point(c: char) -> i32 {
    if c.is_uppercase() {
        ((c as u8) - b'A' + 27) as i32
    } else {
        ((c as u8) - b'a' + 1) as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = lib::read_input("input.test");
        let expected_result = 157;

        let result = part_1(&test_input);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = lib::read_input("input.test");
        let expected_result = 70;

        let result = part_2(&test_input);

        assert_eq!(expected_result, result);
    }
}
