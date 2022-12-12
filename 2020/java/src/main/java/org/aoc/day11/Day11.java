package org.aoc.day11;

import org.aoc.utils.InputReader;

import java.util.ArrayList;

public class Day11 {

    public static int seatGridWidth;
    public static int seatGridHeight;

    public static void main(String[] args) {
        ArrayList<String> input = InputReader.readInputFile("day11");

        char[][] seatGrid = convertToGrid(input);

        seatGridHeight = seatGrid.length;
        seatGridWidth = seatGrid[0].length;

        System.out.printf("Part 1 : %d\n", runSimulation(seatGrid, 4, true));

        System.out.printf("Part 2 : %d\n", runSimulation(seatGrid, 5, false));
    }

    private static int runSimulation(char[][] seatGrid, int threshold, boolean adjacentOnly) {
        boolean seatChanged = true;

        while (seatChanged) {
            boolean stateChangedFlag = false;
            char[][] newSeatGrid = deepCopy(seatGrid);

            for (int i = 0; i < seatGrid.length; i++) {
                for (int j = 0; j < seatGrid[1].length; j++) {
                    switch (seatGrid[i][j]) {
                        case 'L':
                            if (countOccupiedVisibleSeat(seatGrid, i, j, adjacentOnly) == 0) {
                                newSeatGrid[i][j] = '#';
                                stateChangedFlag = true;
                            }
                            break;
                        case '#':
                            if (countOccupiedVisibleSeat(seatGrid, i, j, adjacentOnly) >= threshold) {
                                newSeatGrid[i][j] = 'L';
                                stateChangedFlag = true;
                            }
                            break;
                    }
                }
            }

            if (!stateChangedFlag) {
                seatChanged = false;
            }

            seatGrid = newSeatGrid;
        }

        int count = 0;
        for (char[] row : seatGrid) {
            for (char seat : row) {
                if (seat == '#') {
                    count++;
                }
            }
        }

        return count;
    }

    private static int countOccupiedVisibleSeat(char[][] seatGrid, int posY, int posX, boolean adjacentOnly) {
        int count = 0;
        if (up(seatGrid, posY, posX, adjacentOnly)) {
            // Up
            count++;
        }
        if (down(seatGrid, posY, posX, adjacentOnly)) {
            // Up
            count++;
        }
        if (right(seatGrid, posY, posX, adjacentOnly)) {
            // Up
            count++;
        }
        if (left(seatGrid, posY, posX, adjacentOnly)) {
            // Up
            count++;
        }
        if (upperLeft(seatGrid, posY, posX, adjacentOnly)) {
            // Up
            count++;
        }
        if (upperRight(seatGrid, posY, posX, adjacentOnly)) {
            // Up
            count++;
        }
        if (lowerLeft(seatGrid, posY, posX, adjacentOnly)) {
            // Up
            count++;
        }
        if (lowerRight(seatGrid, posY, posX, adjacentOnly)) {
            // Up
            count++;
        }

        return count;
    }

    private static boolean up(char[][] seatGrid, int posY, int posX, boolean adjacentOnly) {
        for (int i = 1; posY - i >= 0; i++) {
            char seat = seatGrid[posY - i][posX];
            if (seat == '#') {
                return true;
            } else if (seat == 'L') {
                return false;
            } if (adjacentOnly) {
                return false;
            }
        }

        return false;
    }

    private static boolean down(char[][] seatGrid, int posY, int posX, boolean adjacentOnly) {
        for (int i = 1; posY + i < seatGridHeight; i++) {
            char seat = seatGrid[posY + i][posX];
            if (seat == '#') {
                return true;
            } else if (seat == 'L') {
                return false;
            } else if (adjacentOnly) {
                return false;
            }
        }

        return false;
    }

    private static boolean left(char[][] seatGrid, int posY, int posX, boolean adjacentOnly) {
        for (int i = 1; posX - i >= 0; i++) {
            char seat = seatGrid[posY][posX - i];
            if (seat == '#') {
                return true;
            } else if (seat == 'L') {
                return false;
            } else if (adjacentOnly) {
                return false;
            }
        }

        return false;
    }

    private static boolean right(char[][] seatGrid, int posY, int posX, boolean adjacentOnly) {
        for (int i = 1; posX + i < seatGridWidth; i++) {
            char seat = seatGrid[posY][posX + i];
            if (seat == '#') {
                return true;
            } else if (seat == 'L') {
                return false;
            } else if (adjacentOnly) {
                return false;
            }
        }

        return false;
    }

    private static boolean upperRight(char[][] seatGrid, int posY, int posX, boolean adjacentOnly) {
        for (int i = 1; posX + i < seatGridWidth && posY - i >= 0; i++) {
            char seat = seatGrid[posY - i][posX + i];
            if (seat == '#') {
                return true;
            } else if (seat == 'L') {
                return false;
            } else if (adjacentOnly) {
                return false;
            }
        }

        return false;
    }

    private static boolean lowerRight(char[][] seatGrid, int posY, int posX, boolean adjacentOnly) {
        for (int i = 1; posX + i < seatGridWidth && posY + i < seatGridHeight; i++) {
            char seat = seatGrid[posY + i][posX + i];
            if (seat == '#') {
                return true;
            } else if (seat == 'L') {
                return false;
            } else if (adjacentOnly) {
                return false;
            }
        }

        return false;
    }

    private static boolean upperLeft(char[][] seatGrid, int posY, int posX, boolean adjacentOnly) {
        for (int i = 1; posX - i >= 0 && posY - i >= 0; i++) {
            char seat = seatGrid[posY - i][posX - i];
            if (seat == '#') {
                return true;
            } else if (seat == 'L') {
                return false;
            } else if (adjacentOnly) {
                return false;
            }
        }

        return false;
    }

    private static boolean lowerLeft(char[][] seatGrid, int posY, int posX, boolean adjacentOnly) {
        for (int i = 1; posX - i >= 0 && posY + i < seatGridHeight; i++) {
            char seat = seatGrid[posY + i][posX - i];
            if (seat == '#') {
                return true;
            } else if (seat == 'L') {
                return false;
            } else if (adjacentOnly) {
                return false;
            }
        }

        return false;
    }

    private static char[][] deepCopy(char[][] initialArray) {
        return java.util.Arrays.stream(initialArray).map(char[]::clone).toArray($ -> initialArray.clone());
    }

    private static char[][] convertToGrid(ArrayList<String> input) {
        char[][] seatGrid = new char[input.size()][input.get(0).length()];
        int i = 0;
        for (String s : input) {
            seatGrid[i] = s.toCharArray();
            i++;
        }

        return seatGrid;
    }
}
