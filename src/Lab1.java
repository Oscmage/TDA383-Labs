import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.awt.*;
import java.util.HashMap;
import java.util.concurrent.Semaphore;

public class Lab1 {
    private TSimInterface tsi;
    private Semaphore s1;
    private HashMap<Point, Semaphore> hashPoint;

    public Lab1(Integer speed1, Integer speed2) {
        tsi = TSimInterface.getInstance();
        hashPoint = new HashMap<Point, Semaphore>();
        addToHashMap();

        try {
            //manually set the switch
            tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
            tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
//            tsi.setSwitch(3,11,TSimInterface.SWITCH_RIGHT);
        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        }

        Thread t1 = new Thread(new Train(Direction.DOWN, speed1, 1));
        Thread t2 = new Thread(new Train(Direction.UP, speed2, 2));
        t1.start();
        t2.start();

        try {
            t1.join();
            t2.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

    }

    public void addToHashMap() {
        Point p1 = new Point(15,3); // Start punkt 1 uppe
        Point p2 = new Point(15,5); // Start punkt 2 uppe
        Point p3 = new Point(14, 11); // Start punkt 3 nere
        Point p4 = new Point(14, 13); // Start punkt 4 nere

        Semaphore s1 = new Semaphore(1);
        Semaphore s2 = new Semaphore(1);
        Semaphore s3 = new Semaphore(1);
        Semaphore s4 = new Semaphore(1);
        Semaphore s5 = new Semaphore(1);
        Semaphore s6 = new Semaphore(1);

        //Fyrkorsningen längst upp
        hashPoint.put(new Point(6,6),s1);
        hashPoint.put(new Point(9,5),s1);
        hashPoint.put(new Point(10,7),s1);
        hashPoint.put(new Point(10,8),s1);

        //Path längst upp
        hashPoint.put(new Point(14,8),s2);

        //Path höger // Glöm inte att även titta på 14,8 och 13,10. Dessa har redan semaphorer så kan inte läggas till igen.
        hashPoint.put(new Point(14,7),s3);
        hashPoint.put(new Point(13,9),s3);

        //Path mitten
        hashPoint.put(new Point(13,10),s4);
        hashPoint.put(new Point(6,10),s4);

        //Path vänster, Gäller även för 3,13 och 6.10 precis som för path höger
        hashPoint.put(new Point(6,9),s5);
        hashPoint.put(new Point(5,11),s5);

        //Station nedre, av nedre uppe eller nere.
        hashPoint.put(new Point(3,13),s6);
    }

    private enum Direction {
        UP, DOWN
    }

    private class Train implements Runnable {
        private Direction direction;
        private int speed, id;

        public Train(Direction d, int speed, int id) {
            this.direction = d;
            this.speed = speed;
            this.id = id;
        }

        private void setSpeed(int speed) {
            try {
                tsi.setSpeed(this.id, speed);
//                System.out.println("Train id:" + this.id + ", new speed is: " + speed);
            } catch (CommandException e) {
                e.printStackTrace();
            }
        }

        private void sleep() {
            try {
                Thread.sleep(1000 + (20 * Math.abs(this.speed)));
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        @Override
        public void run() {
            setSpeed(speed);
            while (true) {
                try {
                    SensorEvent e = tsi.getSensor(this.id);
                    System.out.println("Sensor for " + this.id + ". " + e.getStatus());
                    handleEvent(e);


                } catch (CommandException e1) {
                    e1.printStackTrace();
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                }
            }

        }

        public void handleEvent(SensorEvent e) {
            Point currPoint = new Point(e.getXpos(), e.getYpos());
            System.out.println("In handleEvent.");
            
        }


    }
}


//Sensorer
//6 6, 9 5, 10 7, 10 8