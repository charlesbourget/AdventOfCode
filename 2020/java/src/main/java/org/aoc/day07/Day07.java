package org.aoc.day7;

import org.aoc.utils.InputReader;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day7 {

    public static void main(String[] args) {
        ArrayList<String> input = InputReader.readInputFile("day07");

        int result1 = part1(input);
        System.out.println("Part 1 : " + result1);

        int result2 = part2(input);
        System.out.println("Part 2 : " + result2);
    }

    private static int part1(ArrayList<String> input) {
        HashMap<String, ArrayList<String>> bagRules = new HashMap<>();

        for (String line : input) {
            String[] bags = line.replaceAll("(s\\b| bag| bags|\\.|\\d+ )", "")
                    .split(" contain ");

            if (!bags[1].contains("no other")) {
                for (String containedBag : bags[1].split(", ")) {
                    bagRules.computeIfAbsent(containedBag, k -> new ArrayList<>()).add(bags[0]);
                }
            }
        }

        HashSet<String> countBagColour = new HashSet<>();
        for (String bag : bagRules.get("shiny gold")) {
            countContainingBags(bag, bagRules, countBagColour);
        }

        return countBagColour.size();
    }

    private static int part2(ArrayList<String> input) {
        HashMap<String, String[]> bagRules = new HashMap<>();

        for (String line : input) {
            String[] bags = line.replaceAll("(s\\b| bag| bags|\\.)", "")
                    .split(" contain ");

            if (!bags[1].contains("no other")) {
                bagRules.put(bags[0], bags[1].split(", "));
            }
        }

        Pattern digitPattern = Pattern.compile("\\d+");
        // Minus one because the shiny gold bag is not counted
        return findNumberOfContainedBag("shiny gold", bagRules, digitPattern) - 1;
    }

    private static void countContainingBags(String bag, HashMap<String, ArrayList<String>> bagRules,
                                            HashSet<String> countBagColour) {
        countBagColour.add(bag);
        ArrayList<String> containingBags = bagRules.get(bag);
        if (containingBags != null) {
            for (String containingBag : containingBags) {
                countContainingBags(containingBag, bagRules, countBagColour);
            }
        }
    }

    private static int findNumberOfContainedBag(String bagName, HashMap<String, String[]> bagRules, Pattern pattern) {
        int count = 1;
        String[] containedBags = bagRules.get(bagName);
        if (containedBags != null) {
            for (String bag : containedBags) {
                Matcher matcher = pattern.matcher(bag);
                if (matcher.find()) {
                    int numberOfBags = Integer.parseInt(matcher.group(0));
                    count += numberOfBags * findNumberOfContainedBag(bag.replaceAll("\\d+ ", ""), bagRules, pattern);
                }
            }
        }
        return count;
    }

}
