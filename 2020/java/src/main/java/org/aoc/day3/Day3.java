package org.aoc.day3;

import org.aoc.utils.InputReader;

import java.util.ArrayList;

public class Day3 {
    private final static int EMPTY = 0, TREE = 1;

    public static void main(String[] args) {
        ArrayList<String> input = InputReader.readInputFile("day03");

        long count;

        Integer[][] terrainMap = createTerrainMap(input);

        count = part1(terrainMap, 3, 1);
        System.out.println("Part 1 : " + count);

        // Part2
        count = count * part1(terrainMap, 1, 1);
        count = count * part1(terrainMap, 5, 1);
        count = count * part1(terrainMap, 7, 1);
        count = count * part1(terrainMap, 1, 2);

        System.out.println("Part 2 : " + count);

    }

    private static int part1(Integer[][] terrainMap, int incrementX, int incrementY) {
        int positionX = 0, positionY = 0;
        int numTree = 0;

        while (positionY < terrainMap.length) {
            if (positionX >= terrainMap[positionY].length) {
                positionX = positionX - terrainMap[positionY].length;
            }

            if (terrainMap[positionY][positionX] == TREE) {
                numTree++;
            }

            positionX += incrementX;
            positionY += incrementY;
        }

        return numTree;
    }

    private static Integer[][] createTerrainMap(ArrayList<String> input) {
        Integer[][] terrainMap = new Integer[input.size()][input.get(0).length()];

        for (int i = 0; i < input.size(); i++) {
            String line = input.get(i);
            for (int j = 0; j < line.length(); j++) {
                terrainMap[i][j] = line.charAt(j) == '.' ? EMPTY : TREE;
            }
        }

        return terrainMap;
    }
}
