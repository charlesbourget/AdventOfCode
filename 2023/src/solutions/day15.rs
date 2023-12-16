use std::collections::HashMap;

use crate::solutions::Parts;
use anyhow::Result;

use super::utils::read_input_str;

const INPUT: &str = include_str!("../../inputs/day15/input.txt");

pub fn run(parts: Parts) -> Result<()> {
    println!("Day 15");
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
    let response = input[0].split(',').map(hash).sum();

    println!("\tPart 1: {}", response);

    Ok(response)
}

fn part_2(input: &[String]) -> Result<i32> {
    let mut map: HashMap<i32, Vec<Value>> = HashMap::new();

    input[0].split(',').for_each(|op| match op.contains('=') {
        true => add(op, &mut map),
        false => remove(op, &mut map),
    });

    let response = map
        .into_iter()
        .flat_map(|(key, values)| {
            values
                .iter()
                .enumerate()
                .map(|(index, value)| (key + 1) * ((index as i32) + 1) * value.value)
                .collect::<Vec<i32>>()
        })
        .sum();

    println!("\tPart 2: {}", response);

    Ok(response)
}

#[derive(Debug)]
struct Value {
    raw_key: String,
    key: i32,
    value: i32,
}

impl Value {
    fn from_str(s: &str) -> Self {
        let (raw_key, raw_value) = s.split_once('=').unwrap();
        let key = hash(raw_key);

        Value {
            raw_key: raw_key.to_owned(),
            key,
            value: raw_value.parse::<i32>().unwrap(),
        }
    }
}

fn hash(key: &str) -> i32 {
    key.to_owned()
        .into_bytes()
        .iter()
        .fold(0, |current, char| ((current + *char as i32) * 17) % 256)
}

fn add(op: &str, map: &mut HashMap<i32, Vec<Value>>) {
    let value = Value::from_str(op);

    match map.get_mut(&value.key) {
        Some(values) => replace_or_add(values, value),
        None => {
            map.insert(value.key, vec![value]);
        }
    }
}

fn replace_or_add(values: &mut Vec<Value>, current_value: Value) {
    match values
        .iter_mut()
        .find(|o| o.raw_key == current_value.raw_key)
    {
        Some(value) => {
            value.value = current_value.value;
        }
        None => {
            values.push(current_value);
        }
    }
}

fn remove(op: &str, map: &mut HashMap<i32, Vec<Value>>) {
    let raw_key = op.replace('-', "");
    let key = hash(&raw_key);

    if let Some(v) = map.get_mut(&key) {
        v.retain(|value| value.raw_key != raw_key);
    }
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use crate::solutions::utils::read_input;
    use test::Bencher;

    #[test]
    fn part_1_test() {
        let test_input = read_input("inputs/day15/input.test").unwrap();
        let expected_result = 1320;

        let result = part_1(&test_input).unwrap();

        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("inputs/day15/input.test").unwrap();
        let expected_result = 145;

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
