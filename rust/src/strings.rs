pub mod operations {
    use std::fmt::Debug;
    use std::str::FromStr;

    pub fn split_into_tuple<T: FromStr + Debug>(input: &str, separator: char) -> (T, T)
    where
        <T as FromStr>::Err: Debug,
    {
        std::println!("input: {:?}", input);
        let mut bounds = input.split(separator);
        let lower_bound = bounds.next().unwrap().parse::<T>().unwrap();
        let upper_bound = bounds.next().unwrap().parse::<T>().unwrap();

        (lower_bound, upper_bound)
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn split_into_u32_tuple_test() {
            let test_input = "4-4";
            let expected_result = (4, 4);

            let result: (u32, u32) = split_into_tuple(test_input, '-');

            assert_eq!(expected_result, result);
        }

        #[test]
        fn split_into_str_tuple_test() {
            let test_input = "re-re";
            let expected_result = (String::from("re"), String::from("re"));

            let result: (String, String) = split_into_tuple(test_input, '-');

            assert_eq!(expected_result, result);
        }
    }
}
