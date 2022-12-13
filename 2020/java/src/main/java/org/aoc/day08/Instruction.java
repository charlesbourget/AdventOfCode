package org.aoc.day8;

public class Instruction {
    private final Operation operation;
    private final int argument;
    private boolean visited;

    public Instruction(Operation operation, int argument) {
        this.operation = operation;
        this.argument = argument;
        visited = false;
    }

    public Operation getOperation() {
        return operation;
    }

    public int getArgument() {
        return argument;
    }

    public boolean isVisited() {
        return visited;
    }

    public void setVisited(boolean visited) {
        this.visited = visited;
    }

    public void visit() {
        visited = true;
    }

    public enum Operation {
        NOP,
        ACC,
        JMP
    }
}
