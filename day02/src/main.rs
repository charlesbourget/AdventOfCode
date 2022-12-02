fn main() {
    let lines = lib::read_input("input.txt");

    let part_1 = part_1(&lines);
    std::println!("Part 1: {}", part_1);

    let part_2 = part_2(&lines);
    std::println!("Part 2: {}", part_2);
}

fn part_1(input: &[String]) -> i32 {
    let mut score = 0;
    for line in input {
        let v: Vec<&str> = line.split(' ').collect();

        score += calculate_score(v[0], v[1]);
    }

    score
}

fn part_2(input: &[String]) -> i32 {
    let mut score = 0;

    for line in input {
        let v: Vec<&str> = line.split(' ').collect();
        let opp = v[0];
        let outcome = v[1];

        let you: &str = match opp {
            "A" => match outcome {
                "X" => "Z",
                "Y" => "X",
                "Z" => "Y",
                &_ => panic!("Invalid outcome"),
            },
            "B" => match outcome {
                "X" => "X",
                "Y" => "Y",
                "Z" => "Z",
                &_ => panic!("Invalid outcome"),
            },
            "C" => match outcome {
                "X" => "Y",
                "Y" => "Z",
                "Z" => "X",
                &_ => panic!("Invalid outcome"),
            },
            &_ => panic!("Invalid opponent"),
        };

        let temp_score = calculate_score(opp, you);
        std::println!("{}", temp_score);
        score += calculate_score(opp, you);
    }

    score
}

fn calculate_score(opp: &str, you: &str) -> i32 {
    match opp {
        "A" => match you {
            "X" => 4,
            "Y" => 8,
            "Z" => 3,
            &_ => panic!("Invalid input"),
        },
        "B" => match you {
            "X" => 1,
            "Y" => 5,
            "Z" => 9,
            &_ => panic!("Invalid input"),
        },
        "C" => match you {
            "X" => 7,
            "Y" => 2,
            "Z" => 6,
            &_ => panic!("Invalid input"),
        },
        &_ => panic!("Invalid input"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = lib::read_input("input.test");
        let expected_result = 15;

        let result = part_1(&test_input);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = lib::read_input("input.test");
        let expected_result = 12;

        let result = part_2(&test_input);

        assert_eq!(expected_result, result);
    }
}
