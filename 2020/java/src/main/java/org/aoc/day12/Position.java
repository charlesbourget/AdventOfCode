package org.aoc.day12;

public class Position {

    protected Direction direction;
    protected int x;
    protected int y;

    public Position() {
        this.direction = Direction.EAST;
        x = 0;
        y = 0;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public void moveVertically(int value) {
        y += value;
    }

    public void moveHorizontally(int value) {
        x += value;
    }

    public void turnLeft(int value) {
        for (int i = 0; i < value / 90; i++) {
            direction = direction.previous();
        }
    }

    public void turnRight(int value) {
        for (int i = 0; i < value / 90; i++) {
            direction = direction.next();
        }
    }

    public void forward(int value) {
        switch (direction) {
            case NORTH -> moveVertically(value);
            case SOUTH -> moveVertically(-1 * value);
            case EAST -> moveHorizontally(value);
            case WEST -> moveHorizontally(-1 * value);
        }
    }

    private enum Direction {

        NORTH{
            @Override
            public Direction previous() {
                return values()[3];
            }
        },
        EAST,
        SOUTH,
        WEST {
            @Override
            public Direction next() {
                return values()[0];
            }
        };

        public Direction next() {
            return values()[ordinal() + 1];
        }

        public Direction previous() {
            return values()[ordinal() - 1];
        }
    }

}
