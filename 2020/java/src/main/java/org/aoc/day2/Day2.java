package org.aoc.day2;

import org.aoc.utils.InputReader;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

public class Day2 {
    private static final int MIN = 0, MAX = 1, LETTER = 2, PASSWORD = 3;

    public static void main(String[] args) {
        ArrayList<String> input = InputReader.readInputFile("day02");

        ArrayList<String[]> formattedInput = input.stream()
                .map(Day2::formatInput)
                .collect(Collectors.toCollection(ArrayList::new));

        part1(formattedInput);
        part2(formattedInput);
    }

    private static void part1(ArrayList<String[]> input) {
        AtomicInteger numberOfPassword = new AtomicInteger();
        input.forEach(val -> {
            int count = 0;
            for (int i = 0; i < val[PASSWORD].length(); i++) {
                if (val[LETTER].equals(String.valueOf(val[PASSWORD].charAt(i)))) {
                    count ++;
                }
            }

            if (count >= Integer.parseInt(val[MIN]) && count <= Integer.parseInt(val[MAX])) {
                numberOfPassword.getAndIncrement();
            }
        });

        System.out.println("Part 1 : " + numberOfPassword);
    }

    private static void part2(ArrayList<String[]> input) {
        AtomicInteger numberOfPassword = new AtomicInteger();
        input.forEach(val -> {
            int firstPosition = Integer.parseInt(val[MIN]) - 1;
            int secondPosition = Integer.parseInt(val[MAX]) - 1;

            String letterFirstPosition = String.valueOf(val[PASSWORD].charAt(firstPosition));
            String letterSecondPosition = String.valueOf(val[PASSWORD].charAt(secondPosition));

            if (val[LETTER].equals(letterFirstPosition)) {
                if (!val[LETTER].equals(letterSecondPosition)) {
                    numberOfPassword.getAndIncrement();
                }
            } else if (val[LETTER].equals(letterSecondPosition)) {
                numberOfPassword.getAndIncrement();
            }
        });

        System.out.println("Part 2 : " + numberOfPassword);
    }

    private static String[] formatInput(String in) {
        String[] out = in.split(" ");

        out[1] = out[1].replace(":", "");

        String[] minMax = out[0].split("-");

        return new String[]{
                minMax[0],
                minMax[1],
                out[1],
                out[2]
        };
    }
}
