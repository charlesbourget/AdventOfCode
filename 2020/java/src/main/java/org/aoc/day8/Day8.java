package org.aoc.day8;

import org.aoc.utils.InputReader;

import java.util.ArrayList;
import java.util.stream.Collectors;

import static org.aoc.day8.Instruction.Operation.*;

public class Day8 {

    public static void main(String[] args) {
        ArrayList<String> input = InputReader.readInputFile("day08");

        ArrayList<Instruction> instructions = parseBootCode(input);

        int result1 = runPart1(instructions);
        System.out.println("Part 1 : " + result1);

        for (int i = 0; i < instructions.size(); i++) {
            ArrayList<Instruction> modified = new ArrayList<>(instructions);
            Instruction instruction = modified.get(i);

            if (instruction.getOperation() == JMP || (instruction.getOperation() == NOP && instruction.getArgument() != 0)) {
                // New Instruction to prevent modifying the original list
                Instruction newInstruction = switch (instruction.getOperation()) {
                    case JMP -> new Instruction(NOP, instruction.getArgument());
                    case NOP -> new Instruction(JMP, instruction.getArgument());
                    case ACC -> new Instruction(ACC, instruction.getArgument());
                };

                modified.set(i, newInstruction);

                Integer result2 = runPart2(modified);

                if (result2 != null) {
                    System.out.println("Part 2 : " + result2);
                }
            }
        }

    }

    public static int runPart1(ArrayList<Instruction> instructions) {
        int accumulator = 0;
        int pointer = 0;

        Instruction instruction = instructions.get(pointer);

        while (!instruction.isVisited()) {
            instruction.visit();
            switch (instruction.getOperation()) {
                case JMP -> pointer += instruction.getArgument();
                case ACC -> {
                    pointer++;
                    accumulator += instruction.getArgument();
                }
                case NOP -> pointer++;
            }
            instruction = instructions.get(pointer);
        }

        return accumulator;
    }

    public static Integer runPart2(ArrayList<Instruction> instructions) {
        int accumulator = 0;
        int pointer = 0;
        // Set all the instruction to unvisited
        instructions.forEach(i -> i.setVisited(false));

        while (pointer < instructions.size()) {
            Instruction instruction = instructions.get(pointer);
            if (instruction.isVisited()) {
                return null;
            }
            switch (instruction.getOperation()) {
                case JMP -> pointer += instruction.getArgument();
                case ACC -> {
                    pointer++;
                    accumulator += instruction.getArgument();
                }
                case NOP -> pointer++;
            }
            instruction.visit();
        }

        return accumulator;
    }

    private static ArrayList<Instruction> parseBootCode(ArrayList<String> input) {
        return input.stream().map(s -> s.split(" "))
                .map(a -> new Instruction(getOperation(a[0]), Integer.parseInt(a[1])))
                .collect(Collectors.toCollection(ArrayList::new));
    }

    private static Instruction.Operation getOperation(String s) {
        return switch (s) {
            case "nop" -> NOP;
            case "acc" -> ACC;
            case "jmp" -> JMP;
            default -> throw new IllegalStateException("Unexpected value: " + s);
        };
    }
}
