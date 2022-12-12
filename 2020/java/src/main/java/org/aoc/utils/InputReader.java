package org.aoc.utils;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class InputReader {

    public static ArrayList<String> readInputFile(String day) {
        ArrayList<String> result = new ArrayList<>();

//        InputStream is = InputReader.class.getClassLoader().getResourceAsStream("input/" + fileName);
        try(BufferedReader bf = new BufferedReader(new FileReader("../inputs/" + day + "/input.txt"))) {
            while (bf.ready()) {
                result.add(bf.readLine());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return result;
    }

    public static List<Integer> convertFileContentToInt(List<String> input) {
        return input.stream().map(Integer::parseInt).collect(Collectors.toCollection(ArrayList::new));
    }

    public static List<Long> convertFileContentToLong(List<String> input) {
        return input.stream().map(Long::parseLong).collect(Collectors.toCollection(ArrayList::new));
    }

}
