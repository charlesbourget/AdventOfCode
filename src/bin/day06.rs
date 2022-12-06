use aoc2022::read_input;

fn main() {
    let lines = read_input("inputs/day06/input.txt");

    let part_1 = part_1(&lines);
    std::println!("Part 1: {}", part_1);

    let part_2 = part_2(&lines);
    std::println!("Part 2: {}", part_2);
}

fn part_1(input: &[String]) -> u32 {
    find_marker(&input[0], 4)
}

fn part_2(input: &[String]) -> u32 {
    find_marker(&input[0], 14)
}

fn find_marker(s: &str, length: u32) -> u32 {
    let char_array = s.chars();

    for i in 0..char_array.count() {
        let substring = &s[i..i + (length as usize)];

        match unique(substring) {
            None => return (i as u32) + length,
            Some(_) => continue,
        }
    }

    0
}

fn unique(s: &str) -> Option<(usize, usize, char)> {
    s.chars().enumerate().find_map(|(i, c)| {
        s.chars()
            .enumerate()
            .skip(i + 1)
            .find(|(_, other)| c == *other)
            .map(|(j, _)| (i, j, c))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day06/input.test");
        let expected_result = 7;

        let result = part_1(&test_input);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day06/input.test");
        let expected_result = 19;

        let result = part_2(&test_input);

        assert_eq!(expected_result, result);
    }
}
