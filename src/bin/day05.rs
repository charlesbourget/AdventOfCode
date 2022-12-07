use aoc2022::read_input;

fn main() {
    let lines = read_input("inputs/day05/input.txt");

    let part_1 = part_1(&lines, 9);
    std::println!("Part 1: {}", part_1);

    let part_2 = part_2(&lines, 9);
    std::println!("Part 2: {}", part_2);
}

fn part_1(input: &[String], num_stacks: u32) -> String {
    let (ref mut stacks, starting_line) = parse_stacks(input, num_stacks);

    for line in input.iter().skip(starting_line) {
        let (number, from, to) = parse_instruction(line);

        for _ in 0..number {
            let value = stacks.get_mut(from).unwrap().pop().unwrap();
            stacks.get_mut(to).unwrap().push(value);
        }
    }

    let mut result = String::new();
    for i in 0..stacks.len() {
        result.push(stacks.get_mut(i as usize).unwrap().pop().unwrap());
    }

    result
}

fn part_2(input: &[String], num_stacks: u32) -> String {
    let (ref mut stacks, starting_line) = parse_stacks(input, num_stacks);

    for line in input.iter().skip(starting_line) {
        let (number, from, to) = parse_instruction(line);

        let from_stack = stacks.get_mut(from).unwrap();
        let mut temp_stack = from_stack.split_off(from_stack.len() - number);
        stacks.get_mut(to).unwrap().append(&mut temp_stack);
    }

    let mut result = String::new();
    for i in 0..stacks.len() {
        result.push(stacks.get_mut(i as usize).unwrap().pop().unwrap());
    }

    result
}

fn parse_stacks(input: &[String], num_stacks: u32) -> (Vec<Vec<char>>, usize) {
    let mut stacks = Vec::new();

    for _ in 0..num_stacks {
        stacks.push(Vec::new());
    }

    for (j, line) in input.iter().enumerate() {
        if line.starts_with(" 1") {
            return (stacks, (j + 2) as usize);
        }
        let char_array: Vec<char> = line.chars().collect();

        for column in 0..num_stacks {
            let index = (column * 4 + 1) as usize;
            let data = char_array.get(index).unwrap();

            if data != &' ' {
                let stack = stacks.get_mut(column as usize).unwrap();
                stack.insert(0, *data);
            }
        }
    }

    (stacks, 0)
}

fn parse_instruction(line: &str) -> (usize, usize, usize) {
    let mut iter = line.split(' ');

    let number = iter.nth(1).unwrap().parse::<usize>().unwrap();
    let from = iter.nth(1).unwrap().parse::<usize>().unwrap() - 1;
    let to = iter.nth(1).unwrap().parse::<usize>().unwrap() - 1;

    (number, from, to)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day05/input.test");
        let expected_result = "CMZ";

        let result = part_1(&test_input, 3);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day05/input.test");
        let expected_result = "MCD";

        let result = part_2(&test_input, 3);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn parse_stacks_test() {
        let test_input = read_input("inputs/day05/input.test");
        let expected_result = (vec![vec!['Z', 'N'], vec!['M', 'C', 'D'], vec!['P']], 5);

        let result = parse_stacks(&test_input, 3);

        assert_eq!(expected_result, result);
    }
}
