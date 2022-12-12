package org.aoc.day10;

import org.aoc.utils.InputReader;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

public class Day10 {

    private static int count1;
    private static int count3;

    public static void main(String[] args) {
        ArrayList<Integer> input = InputReader.convertFileContentToInt(InputReader.readInputFile("day10"))
                .stream().sorted().collect(Collectors.toCollection(ArrayList::new));

        count1 = 0;
        count3 = 0;

        HashSet<Integer> adapters = new HashSet<>(input);

        int deviceJoltage = Collections.max(input) + 3;

        int result1 = part1(adapters, deviceJoltage);
        System.out.println("Part 1 : " + result1);

        AtomicLong configurations = new AtomicLong(1);
        AtomicInteger onesStreak = new AtomicInteger();

        input.stream().reduce(0, (prev, next) -> {
            part2(prev, next, onesStreak, configurations);
            return next;
        });

        part2(0, 0, onesStreak, configurations);
        System.out.println("Part 2 : " + configurations.get());
    }

    private static int part1(HashSet<Integer> input, int deviceJoltage) {
        int startingJoltage = 0;
        int gap = 1;
        findNextAdapter(startingJoltage, gap, input, deviceJoltage);
        return count1 * count3;
    }

    private static void part2(int prev, int next, AtomicInteger onesStreak, AtomicLong configurations) {
        if (next - prev == 1) {
            onesStreak.incrementAndGet();
        } else {
            configurations.set(configurations.get() * getConfigurationsCount(onesStreak.get()));
            onesStreak.set(0);
        }
    }

    /**
     * Implementation of the Lazy Caterer's sequence
     * https://en.wikipedia.org/wiki/Lazy_caterer%27s_sequence
     *
     * @param streak Number of cuts
     * @return Number of pieces that can be created
     */
    private static long getConfigurationsCount(int streak) {
        streak -= 1;
        return (long) (Math.pow(streak, 2.0) + streak + 2) / 2;
    }

    private static void findNextAdapter(int startingJoltage, int gap, HashSet<Integer> adapters, int deviceJoltage) {
        if (startingJoltage + 3 == deviceJoltage) {
            incrementCount(3);
        } else if (adapters.contains(startingJoltage + gap)) {
            incrementCount(gap);
            findNextAdapter(startingJoltage + gap, 1, adapters, deviceJoltage);
        } else if (gap <= 2) {
            findNextAdapter(startingJoltage, gap + 1, adapters, deviceJoltage);
        }
    }

    private static void incrementCount(int gap) {
        switch (gap) {
            case 1 -> count1++;
            case 3 -> count3++;
        }

    }
}
