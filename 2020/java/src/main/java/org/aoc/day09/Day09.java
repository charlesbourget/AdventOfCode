package org.aoc.day9;

import org.aoc.utils.InputReader;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Day9 {

    public static void main(String[] args) {
        List<Long> input = InputReader.convertFileContentToLong(InputReader.readInputFile("day09"));

        long invalidNumber = part1(input);
        System.out.println("Part 1 : " + invalidNumber);

        long sum = part2(input, invalidNumber);
        System.out.println("Part 1 : " + sum);
    }

    private static long part1(List<Long> input) {
        for (int i = 25; i < input.size(); i++) {
            List<Long> preamble = input.subList(i - 25, i);
            if (!validateNumber(input.get(i), preamble)) {
                return input.get(i);
            }
        }

        return 0;
    }

    private static long part2(List<Long> input, long invalidNumber) {
        for (int i = 0; i < input.size(); i++) {
            if (i < invalidNumber) {
                for (int j = i + 1; j < input.size(); j++) {
                    List<Long> numbers = input.subList(i, j + 1);
                    if (numbers.stream().mapToLong(a -> a).sum() == invalidNumber) {
                        return (Collections.max(numbers) + Collections.min(numbers));
                    }
                }
            }
        }

        return 0;
    }

    private static boolean validateNumber(long number, List<Long> preamble) {
        for (int i = 0; i < preamble.size(); i++) {
            for (int j = i + 1; j < preamble.size(); j++) {
                if (preamble.get(i) + preamble.get(j) == number) {
                    return true;
                }
            }
        }

        return false;
    }

}
