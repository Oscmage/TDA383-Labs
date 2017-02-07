import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.HashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Lab2 {
    private TSimInterface tsi;
    private final Lock lock = new ReentrantLock(true);
    private final Condition cross = lock.newCondition();
    private final Condition rightSingleRail = lock.newCondition();
    private Boolean atCross = false;
    private Boolean atRightSingleRail = false;
    private HashMap<Condition,Boolean> conditionToBoolean;
    private Semaphore leftSingleRail,middleDualRail,station1b,station2b;


    public Lab2(Integer speed1, Integer speed2) {
        tsi = TSimInterface.getInstance();

        conditionToBoolean = new HashMap<Condition, Boolean>();
        conditionToBoolean.put(cross, atCross);
        conditionToBoolean.put(rightSingleRail, atRightSingleRail);

        leftSingleRail = new Semaphore(1);
        middleDualRail = new Semaphore(1);
        station1b = new Semaphore(1);
        station2b = new Semaphore(1);

        Thread t1 = new Thread(new Train(Direction.DOWN, speed1, 1));
        Thread t2 = new Thread(new Train(Direction.UP, speed2, 2));
        t1.start();
        t2.start();
    }

    private enum Direction {
        UP, DOWN;

        public static Direction getOpposite (Direction d) {
            return d == UP ? DOWN : UP;
        }
    }

    private class Train implements Runnable {
        private Direction direction;
        private int speed, id;
        private boolean holdsMiddleDualRail = false;

        public Train(Direction d, int originalSpeed, int id) {
            this.direction = d;
            this.speed = originalSpeed;
            this.id = id;
        }

        @Override
        public void run() {

            try {
                tsi.setSpeed(this.id, speed);
            } catch (CommandException e) {
                System.err.println(e.getMessage());
                System.exit(1);
            }

            while (true) {
                SensorEvent e;
                try {
                    e = tsi.getSensor(this.id);
                    handleEvent(e);
                } catch (CommandException e1) {
                    System.err.println(e1.getMessage());
                    System.exit(1);
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                }
            }
        }

        private void sleep() throws InterruptedException {
            Thread.sleep(1000 + (20 * Math.abs(this.speed)));
        }


        private void acquire(Condition c) throws CommandException, InterruptedException {
            tsi.setSpeed(this.id, 0);
            while(conditionToBoolean.get(c)) {
                c.await();
            }
            conditionToBoolean.replace(c, true); // Update the value of the boolean
            tsi.setSpeed(this.id, this.speed);
        }

        private void acquire(Semaphore s) throws InterruptedException, CommandException {
            tsi.setSpeed(this.id, 0);
            s.acquire();
            tsi.setSpeed(this.id, speed);
        }


        private void acquireAndChangeSwitch(Condition c,int x, int y, int direction) throws CommandException, InterruptedException {
            tsi.setSpeed(this.id, 0);
            while(conditionToBoolean.get(c)) {
                c.await();
            }
            conditionToBoolean.replace(c, true); // Update the value of the boolean
            TSimInterface.getInstance().setSwitch(x, y, direction);
            tsi.setSpeed(this.id, this.speed);
        }

        private void acquireAndChangeSwitch(Semaphore s, int x, int y, int direction) throws CommandException,
                InterruptedException
        {
            tsi.setSpeed(this.id ,0);
            s.acquire();
            tsi.setSwitch(x, y, direction);
            tsi.setSpeed(this.id, speed);
        }

        private boolean isDirectionDown() {
            return direction == Direction.DOWN;
        }

        public void handleEvent(SensorEvent e) throws CommandException, InterruptedException {
            if (isSensorActive(e)) {
                int x = e.getXpos();
                int y =  e.getYpos();

                // Handler methods for the different situations that can occur.
                if (handleStartStop(x, y)) { //Takes care of all stations
                    return;
                } else if (handleCross(x, y)) { // Takes care of the cross intersection
                    return;
                } else if (handleRightSingleRail(x, y)) { // Takes care of single rail on the right side of the map
                    return;
                } else if (handleLeftSingleRail(x, y)) { // Takes care of single rail on right side of the map
                    return;
                } else {
                    handleMiddleDualRail(x, y); // Handles the middle dual rail.
                }
            }
        }

        private boolean handleLeftSingleRail(int x, int y) throws CommandException, InterruptedException {

            if (x == 7 && y == 9) { // Currently on middle dual rail, top track.
                if (isDirectionDown()) {
                    acquireAndChangeSwitch(leftSingleRail, 4, 9, TSimInterface.SWITCH_LEFT);
                } else {
                    leftSingleRail.release();
                }
                return true;
            }

            if (x == 6 && y == 10) { // Currently on middle dual rail, bottom track.
                if (isDirectionDown()) {
                    acquireAndChangeSwitch(leftSingleRail, 4, 9, TSimInterface.SWITCH_RIGHT);
                } else {
                    leftSingleRail.release();
                }
                return true;
            }

            if (x == 6 && y == 11) { // Top station (of the bottom ones)
                if (isDirectionDown()) {
                    leftSingleRail.release();
                } else {
                    acquireAndChangeSwitch(leftSingleRail, 3, 11, TSimInterface.SWITCH_LEFT);
                }
                return true;
            }

            if (x == 5 && y == 13) { // Bottom station (of the bottom ones)
                if (isDirectionDown()) {
                    leftSingleRail.release();
                } else {
                    acquireAndChangeSwitch(leftSingleRail, 3, 11, TSimInterface.SWITCH_RIGHT);
                    station2b.release();
                }
                return true;
            }
            return false;
        }

        private void handleMiddleDualRail(int x, int y) throws CommandException {

            // Sensor on the right side of the dual rail in middle
            if (x == 19 && y == 8) {
                if (isDirectionDown()) { // Heading towards the dual rail
                    if (middleDualRail.tryAcquire()) {
                        tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                        this.holdsMiddleDualRail = true;
                    } else {
                        tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    }
                } else { // Heading away from the dual rail and upwards
                    if (holdsMiddleDualRail) {
                        middleDualRail.release();
                        holdsMiddleDualRail = false;
                    }

                    if (station1b.tryAcquire()) {
                        tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                    } else {
                        tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    }
                }
            }

            // Sensor on the left side of the dual rail in middle
            if (x == 1 && y == 10) {
                if (isDirectionDown()) { // Heading away from dual rail
                    if (holdsMiddleDualRail) {
                        middleDualRail.release();
                        holdsMiddleDualRail = false;
                    }

                    if (station2b.tryAcquire()) {
                        tsi.setSwitch(3, 11 ,TSimInterface.SWITCH_RIGHT);
                    } else {
                        tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                    }
                } else { // Heading towards the dual middle rail
                    if (middleDualRail.tryAcquire()) { // Is someone on the bottom track?
                        tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                        this.holdsMiddleDualRail = true;
                    } else {
                        tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                    }
                }
            }
        }

        private boolean handleRightSingleRail(int x, int y) throws CommandException, InterruptedException {
            lock.lock();
            // To the right of the cross
            if (x == 14 && y == 7) { // Top of the two
                if (isDirectionDown()) {

                    tsi.setSpeed(this.id, 0);
                    while(atRightSingleRail) {
                        rightSingleRail.await();
                    }
                    atRightSingleRail = true;
                    TSimInterface.getInstance().setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    tsi.setSpeed(this.id, this.speed);
                } else {
                    atRightSingleRail = false;
                    rightSingleRail.signal();
                }
                lock.unlock();
                return true;
            } else if (x == 14 && y == 8) { // Bottom of the two
                if (isDirectionDown()) {
                    tsi.setSpeed(this.id, 0);
                    while(atRightSingleRail) {
                        rightSingleRail.await();
                    }
                    atRightSingleRail = true;
                    TSimInterface.getInstance().setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                    tsi.setSpeed(this.id, this.speed);
                    station1b.release();
                } else {
                    atRightSingleRail = false;
                    rightSingleRail.signal();
                }
                lock.unlock();
                return true;
            }

            // You're currently on the middle dual rail. Top of the two
            if (x == 12 && y == 9) {
                if (isDirectionDown()) {
                    atRightSingleRail = false;
                    rightSingleRail.signal();
                } else {
                    tsi.setSpeed(this.id, 0);
                    while(atRightSingleRail) {
                        rightSingleRail.await();
                    }
                    atRightSingleRail = true;
                    TSimInterface.getInstance().setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    tsi.setSpeed(this.id, this.speed);
                }
                lock.unlock();
                return true;
            }

            // You're currently on the middle dual rail. Bottom of the two
            if (x == 13 && y == 10) {
                if (this.direction == Direction.UP) {
                    tsi.setSpeed(this.id, 0);
                    while(atRightSingleRail) {
                        rightSingleRail.await();
                    }
                    atRightSingleRail = true;
                    TSimInterface.getInstance().setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    tsi.setSpeed(this.id, this.speed);
                } else {
                    atRightSingleRail = false;
                    rightSingleRail.signal();
                }
                lock.unlock();
                return true;
            }
            lock.unlock();
            return false;
        }

        /**
         * Takes care of the start and stop at the stations.
         */
        private boolean handleStartStop(int x, int y) throws CommandException, InterruptedException {
            if (x == 16 && (y == 3 || y == 5)) { // Top stations
                if (!isDirectionDown()) slowDownAndSwitchDirection(); // Heading upwards
                return true;
            } else if (x == 15 && (y == 11 || y == 13)) { //Bottom stations
                if (isDirectionDown()) slowDownAndSwitchDirection(); // Heading downwards.
                return true;
            }
            return false;
        }

        /**
         * Slows down the train, sleeps and then switches direction.
         */
        private void slowDownAndSwitchDirection() throws CommandException, InterruptedException {
            tsi.setSpeed(this.id, 0);
            sleep();
            this.speed = - this.speed;
            tsi.setSpeed(this.id, this.speed);
            this.direction = Direction.getOpposite(this.direction);
        }

        private boolean handleCross(int x, int y) throws CommandException, InterruptedException {
            lock.lock();
            // You're west or north of the cross.
            if (x == 6 && y == 6 || x == 9 && y == 5) {
                if (isDirectionDown()) {
                    tsi.setSpeed(this.id, 0);
                    while(atCross) {
                        cross.await();
                    }
                    atCross = true;
                    tsi.setSpeed(this.id, this.speed);
                } else {
                    atCross = false;
                    cross.signal();
                }
                lock.unlock();
                return true;
            }

            // You're east or south of the cross.
            if (x == 11 && y == 7 || x == 10 && y == 8) {
                if (isDirectionDown()) {
                    atCross = false;
                    cross.signal();
                } else {
                    tsi.setSpeed(this.id, 0);
                    while(atCross) {
                        cross.await();
                    }
                    atCross = true;
                    tsi.setSpeed(this.id, this.speed);
                }
                lock.unlock();
                return true;
            }
            lock.unlock();
            return false;
        }

        private boolean isSensorActive(SensorEvent e) {
            return e.getStatus() == 1;
        }
    }
}