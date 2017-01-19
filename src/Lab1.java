import TSim.CommandException;
import TSim.TSimInterface;

import java.awt.*;
import java.util.HashMap;
import java.util.concurrent.Semaphore;

public class Lab1 {
    private TSimInterface tsi;
    private Semaphore s1;
    private HashMap<Point,Semaphore> hashPoint;

    public Lab1(Integer speed1, Integer speed2) {
        tsi = TSimInterface.getInstance();
        hashPoint = new HashMap<Point, Semaphore>();
        

        try {
            tsi.setSwitch(17,7,TSimInterface.SWITCH_RIGHT);
            tsi.setSwitch(15,9,TSimInterface.SWITCH_RIGHT);
            tsi.setSwitch(3,11,TSimInterface.SWITCH_RIGHT);
        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        }

        Thread t1 = new Thread(new Train(Direction.DOWN,speed1,1));
        Thread t2 = new Thread(new Train(Direction.UP,speed2,2));
        t1.start();
        t2.start();

        try {
            t1.join();
            t2.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

    }

    private enum Direction {
        UP,DOWN
    }



    private class Train implements Runnable{
        private Direction direction;
        private int speed,id;


        public Train (Direction d, int speed, int id) {
            this.direction = d;
            this.speed = speed;
            this.id = id;
            setSpeed(speed);
        }

        private void setSpeed(int speed) {
            try {
                tsi.setSpeed(this.id,speed);
                System.out.println("Train id:" + this.id + ", new speed is: " + speed);
            } catch (CommandException e) {
                e.printStackTrace();
            }
        }

        private void sleep () {
            try {
                Thread.sleep(1000 + (20 * Math.abs(this.speed)));
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        @Override
        public void run() {

        }
    }
}
