use anyhow::Result;
use quote::quote;

pub fn generate_template(day: &str) -> Result<String> {
    let input_path = format!("inputs/day{}/input.txt", day);
    let test_input_path = format!("inputs/day{}/input.test", day);

    let tokens = quote! {
        use crate::solutions::utils::read_input;
        use crate::solutions::Parts;
        use anyhow::Result;

        pub fn run(parts: Parts) -> Result<()> {
            let lines = read_input(#input_path)?;

            match parts {
                Parts::One => part_1(&lines)?,
                Parts::Two => part_2(&lines)?,
                Parts::Both => {
                    part_1(&lines)?;
                    part_2(&lines)?;
                }
            }

            Ok(())
        }

        fn part_1(input: &[String]) -> Result<()> {
            let response = 0;

            println!("Part 1: {}", response);

            Ok(())
        }

        fn part_2(input: &[String]) -> Result<()> {
            let response = 0;

            println!("Part 2: {}", response);

            Ok(())
        }

        #[cfg(test)]
        mod tests {
            use super::*;

            #[test]
            fn part_1_test() {
                let test_input = read_input(#test_input_path).unwrap();
                let expected_result = 0;

                let result = part_1(&test_input).unwrap();

                assert_eq!(expected_result, result);
            }

            #[test]
            fn part_2_test() {
                let test_input = read_input(#test_input_path).unwrap();
                let expected_result = 0;

                let result = part_2(&test_input).unwrap();

                assert_eq!(expected_result, result);
            }
        }
    };

    let ast = syn::parse2(tokens)?;
    let content = prettyplease::unparse(&ast);

    Ok(content)
}
