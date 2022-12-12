package org.aoc.day5;

import org.aoc.utils.InputReader;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Day5 {
    public static void main(String[] args) {
        ArrayList<String> input = InputReader.readInputFile("day05");

        List<Integer> seatIds = new ArrayList<>();
        int result1 = part1(input, seatIds);
        System.out.println("Part 1 : " + result1);

        int result2 = part2(seatIds);
        System.out.println("Part 2 : " + result2);
    }

    private static int part1(ArrayList<String> input, List<Integer> seatIds) {
        int maxSeatId = 0;
        for (String info : input) {
            String rowInfo = info.substring(0, 7);
            String columnInfo = info.substring(7);

            int row = findNumber(rowInfo, 127, 'F');
            int column = findNumber(columnInfo, 7, 'L');

            int seatId = row * 8 + column;
            if (seatId > maxSeatId) {
                maxSeatId = seatId;
            }

            seatIds.add(seatId);
        }

        return maxSeatId;
    }

    private static int part2(List<Integer> seatIds) {
        int min = Collections.min(seatIds);
        int max = Collections.max(seatIds);

        for (int i = min; i <= max; i++) {
            if (!seatIds.contains(i)) {
                if (seatIds.contains(i - 1) && seatIds.contains(i + 1)) {
                    return i;
                }
            }
        }

        return 0;
    }

    private static int findNumber(String info, int max, char lower) {
        int min = 0;
        for (char letter : info.toCharArray()) {
            if (letter == lower) {
                max = (int) ((max - min + 1) / 2.0) - 1 + min;
            } else {
                min = (int) ((max - min + 1) / 2.0) + min;
            }
        }

        return min;
    }
}
