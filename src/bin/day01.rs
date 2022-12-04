use aoc2022::read_input;

fn main() {
    let lines = read_input("inputs/day01/input.txt");

    let part_1 = part_1(&lines);
    std::println!("Part 1: {}", part_1);

    let part_2 = part_2(&lines);
    std::println!("Part 2: {}", part_2);
}

fn part_1(input: &[String]) -> u32 {
    calculate_cals_per_elf(input)[0]
}

fn part_2(input: &[String]) -> u32 {
    calculate_cals_per_elf(input)[..3].iter().sum()
}

fn calculate_cals_per_elf(input: &[String]) -> Vec<u32> {
    let mut elfs: Vec<u32> = Vec::new();
    let mut current = 0;

    for item in input {
        if item.is_empty() {
            elfs.push(current);
            current = 0;
        } else {
            current += item.parse::<u32>().expect("Could not parse int");
        }
    }

    elfs.push(current);
    elfs.sort_unstable();
    elfs.reverse();

    elfs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day01/input.test");
        let expected_result = 24000;

        let result = part_1(&test_input);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day01/input.test");
        let expected_result = 45000;

        let result = part_2(&test_input);

        assert_eq!(expected_result, result);
    }
}
