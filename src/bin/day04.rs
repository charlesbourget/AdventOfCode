use aoc2022::read_input;

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

        if (elf1.lower <= elf2.lower && elf1.upper >= elf2.upper)
            || (elf2.lower <= elf1.lower && elf2.upper >= elf1.upper)
        {
            count += 1;
        }
    }

    count
}

fn part_2(input: &[String]) -> u32 {
    let mut count = 0;

    for line in input.iter() {
        let (elf1, elf2) = parse_assignements(line);

        if (elf1.lower <= elf2.lower && elf1.upper >= elf2.lower)
            || (elf1.lower <= elf2.upper && elf1.upper >= elf2.upper)
            || (elf2.lower <= elf1.lower && elf2.upper >= elf1.lower)
            || (elf2.lower <= elf1.upper && elf2.upper >= elf1.upper)
        {
            count += 1;
        }
    }

    count
}

fn parse_assignements(line: &str) -> (Assignement, Assignement) {
    let mut assignements = line.split(',');
    let elf1 = calculate_section_bounds(assignements.next().unwrap());
    let elf2 = calculate_section_bounds(assignements.next().unwrap());

    (elf1, elf2)
}

fn calculate_section_bounds(assignement: &str) -> Assignement {
    let mut bounds = assignement.split('-');
    let lower_bound = bounds.next().unwrap().parse::<u32>().unwrap();
    let upper_bound = bounds.next().unwrap().parse::<u32>().unwrap();

    Assignement {
        lower: lower_bound,
        upper: upper_bound,
    }
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
