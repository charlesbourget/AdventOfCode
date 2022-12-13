package org.aoc.day6;

import org.aoc.utils.InputReader;

import java.util.ArrayList;
import java.util.HashMap;

public class Day6 {
    public static void main(String[] args) {
        ArrayList<String> input = InputReader.readInputFile("day06");

        countAnswers(input);
    }

    private static void countAnswers(ArrayList<String> input) {
        long countPart2 = 0;
        long countPart1 = 0;
        int numLines = 0;
        HashMap<Character, Integer> answers = new HashMap<>();
        for (String line : input) {
            if (line.equals("")) {
                int finalNumLines = numLines;
                countPart1 += answers.size();
                countPart2 += answers.entrySet()
                        .stream()
                        .filter(answer -> answer.getValue() == finalNumLines)
                        .count();

                numLines = 0;
                answers = new HashMap<>();
            } else {
                for (char a : line.toCharArray()) {
                    if (!answers.containsKey(a)) {
                        answers.put(a, 1);
                    } else {
                        int newValue = answers.get(a) + 1;
                        answers.replace(a, newValue);
                    }
                }

                numLines++;
            }
        }

        // Count last line
        int finalNumLines = numLines;
        countPart1 += answers.size();
        countPart2 += answers.entrySet()
                .stream()
                .filter(answer -> answer.getValue() == finalNumLines)
                .count();

        System.out.println("Part 1 : " + countPart1);
        System.out.println("Part 2 : " + countPart2);
    }
}
