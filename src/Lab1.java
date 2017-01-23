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
//        Point p1 = new Point(6,6);
//        Point p2 = new Point(9,5);
//        Point p3 = new Point(10,7);
//        Point p4 = new Point(10,8);
        Point p5 = new Point(13, 9);
        Point p6 = new Point(14, 7);
        Point p7 = new Point(13, 10);

//        Semaphore s1 = new Semaphore(1);
        Semaphore s2 = new Semaphore(1);

//        hashPoint.put(p1, s1);
//        hashPoint.put(p2, s1);
//        hashPoint.put(p3, s1);
//        hashPoint.put(p4, s1);
        hashPoint.put(p5, s2);
        hashPoint.put(p6, s2);

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

            if (e.getStatus() == 1) {
                if (hashPoint.containsKey(currPoint)) {
                    Semaphore currSem = hashPoint.get(currPoint);
                    System.out.println("In containsKey, accquiring.");

                    try {
                        this.setSpeed(0);
                        currSem.acquire();
                        System.out.println("The train " + this.id + " accquired.");
                        this.setSpeed(speed);
                        tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    } catch (InterruptedException e1) {
                        e1.printStackTrace();
                    } catch (CommandException e1) {
                        e1.printStackTrace();
                    }

                } else {
                    if (hashPoint.containsKey(currPoint)) {

                        System.out.println("In containsKey, releasing");
                        hashPoint.get(currPoint).release();
                    }
                }
            }


        }


    }
}


//Sensorer
//6 6, 9 5, 10 7, 10 8