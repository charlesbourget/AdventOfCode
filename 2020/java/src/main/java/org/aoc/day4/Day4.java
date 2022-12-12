package org.aoc.day4;

import org.aoc.utils.InputReader;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Day4 {

    private static final String byrField = ".*byr:.*";
    private static final String eyrField = ".*eyr:.*";
    private static final String hgtField = ".*hgt:.*";
    private static final String pidField = ".*pid:.*";
    private static final String iyrField = ".*iyr:.*";
    private static final String eclField = ".*ecl:.*";
    private static final String hclField = ".*hcl:.*";

    private static final List<String> eyeColor = Arrays.asList(
            "amb",
            "blu",
            "brn",
            "gry",
            "grn",
            "hzl",
            "oth"
    );
    private static final List<String> units = Arrays.asList(
            "cm",
            "in"
    );

    public static void main(String[] args) {
        ArrayList<String> input = InputReader.readInputFile("day04");

        ArrayList<String> formattedInput = formatInput(input);

        ArrayList<String> filteredInput = part1(formattedInput);
        System.out.println("Part 1 : " + filteredInput.size());

        int result = part2(filteredInput);
        System.out.println("Part 2 : " + result);
    }

    private static ArrayList<String> part1(ArrayList<String> formattedInput) {
        return formattedInput.stream().filter(passport -> Pattern.matches(byrField, passport) &&
                Pattern.matches(eyrField, passport) &&
                Pattern.matches(hgtField, passport) &&
                Pattern.matches(pidField, passport) &&
                Pattern.matches(iyrField, passport) &&
                Pattern.matches(eclField, passport) &&
                Pattern.matches(hclField, passport)).collect(Collectors.toCollection(ArrayList::new));
    }

    private static int part2(ArrayList<String> filteredInput) {
        AtomicInteger count = new AtomicInteger();
        filteredInput.forEach(passport -> {
            Map<String, String> passportMap = createPassportMap(passport);
            if (validateData(passportMap)) {
                count.getAndIncrement();
            }
        });

        return count.get();
    }

    private static ArrayList<String> formatInput(ArrayList<String> input) {
        ArrayList<String> formattedInput = new ArrayList<>();
        StringBuilder currentPassport = new StringBuilder();

        for (String line : input) {
            if (line.equals("")) {
                formattedInput.add(currentPassport.toString());
                currentPassport = new StringBuilder();
            } else {
                currentPassport.append(line).append(" ");
            }
        }

        formattedInput.add(currentPassport.toString());

        return formattedInput;
    }

    private static Map<String, String> createPassportMap(String passport) {
        Map<String, String> passportMap = new HashMap<>();
        String[] passportFields = passport.split(" ");

        for (String passportField : passportFields) {
            String[] keyVal = passportField.split(":");
            if (!keyVal[1].equals("") && !keyVal[0].equals("cid")) {
                passportMap.put(keyVal[0], keyVal[1]);
            }
        }

        return passportMap;
    }


    private static boolean validateData(Map<String, String> passportMap) {
        if (invalidIntValue("byr", 1920, 2002, passportMap)) {
            return false;
        }
        if (invalidIntValue("iyr", 2010, 2020, passportMap)) {
            return false;
        }
        if (invalidIntValue("eyr", 2020, 2030, passportMap)) {
            return false;
        }

        String heightValue = passportMap.get("hgt");
        String unit = heightValue.substring(heightValue.length() - 2);
        if (!units.contains(unit)) {
            return false;
        }
        int height = Integer.parseInt(heightValue.substring(0, heightValue.length() - 2));

        if (unit.equals("cm")) {
            if (invalidHeight(height, 150, 193)) {
                return false;
            }
        } else {
            if (invalidHeight(height, 59, 76)) {
                return false;
            }
        }

        if (!Pattern.matches("#[0-9,a-f]{6}", passportMap.get("hcl"))) {
            return false;
        }

        if (!eyeColor.contains(passportMap.get("ecl"))) {
            return false;
        }

        if (!Pattern.matches("[0-9]{9}", passportMap.get("pid"))) {
            return false;
        }

        return true;
    }

    private static boolean invalidHeight(int height, int min, int max) {
        return height < min || height > max;
    }

    private static boolean invalidIntValue(String field, int min, int max, Map<String, String> map) {
        int value = Integer.parseInt(map.get(field));
        return value < min || value > max;
    }

}
