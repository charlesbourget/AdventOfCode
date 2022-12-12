package org.aoc.day12;

import org.aoc.utils.InputReader;

import java.util.ArrayList;

public class Day12 {

    public static void main(String[] args) {
        ArrayList<String> input = InputReader.readInputFile("day12");

        Position position1 = new Position();
        System.out.printf("Part 1 : %d\n", advanceShip(input, position1));

        ShipPosition position2 = new ShipPosition();
        System.out.printf("Part 2 : %d\n", advanceShip(input, position2));
    }

    private static int advanceShip(ArrayList<String> input, Position position) {
        for (String instruction : input) {
            runInstruction(instruction, position);
        }

        return Math.abs(position.getX()) + Math.abs(position.getY());
    }

    private static void runInstruction(String input, Position currentPosition) {
        int value = Integer.parseInt(input.substring(1));

        switch (input.charAt(0)) {
            case 'N' -> currentPosition.moveVertically(value);
            case 'S' -> currentPosition.moveVertically(-1 * value);
            case 'E' -> currentPosition.moveHorizontally(value);
            case 'W' -> currentPosition.moveHorizontally(-1 * value);
            case 'L' -> currentPosition.turnLeft(value);
            case 'R' -> currentPosition.turnRight(value);
            case 'F' -> currentPosition.forward(value);
        }
    }
}
