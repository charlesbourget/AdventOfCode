package org.aoc.day12;

public class ShipPosition extends Position{

    private int wx;
    private int wy;

    public ShipPosition() {
        super();
        this.wx = 10;
        this.wy = 1;
    }

    @Override
    public void moveVertically(int value) {
        wy += value;
    }

    @Override
    public void moveHorizontally(int value) {
        wx += value;
    }

    @Override
    public void turnLeft(int value) {
        for (int i = 0; i < value; i +=90) {
            int temp = wx;
            wx = -wy;
            wy = temp;
        }
    }

    @Override
    public void turnRight(int value) {
        for (int i = 0; i < value; i +=90) {
            int temp = wx;
            wx = wy;
            wy = -temp;
        }
    }

    @Override
    public void forward(int value) {
        x += (value * wx);
        y += (value * wy);
    }
}
