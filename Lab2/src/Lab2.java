import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class Lab2 {
    private TSimInterface tsi;
    private final Monitor cross = new Monitor();
    private final Monitor leftSingleRail = new Monitor();
    private final Monitor rightSingleRail = new Monitor();
    private final Monitor middleDualRail = new Monitor();
    private final Monitor station1b = new Monitor();
    private final Monitor station2b = new Monitor();



    public Lab2(Integer speed1, Integer speed2) {
        tsi = TSimInterface.getInstance();

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

    private class Monitor {
        private boolean onTrack;
        private ReentrantLock lock;
        private Condition trackFree;

        public Monitor() {
            onTrack = false;
            lock = new ReentrantLock(true);
            trackFree = lock.newCondition();
        }

        public void enter () {
            lock.lock();
            try {
                while (onTrack) {
                    trackFree.await();
                }
                onTrack = true;
            } catch (InterruptedException e) {
                System.err.print("Problem with the enter method occured: " + e);
            } finally {
                lock.unlock();
            }
        }

        public void leave () {
            lock.lock();
            try {
                onTrack = false;
                trackFree.signal();
            } finally {
                lock.unlock();
            }
        }

        public boolean tryEnter() {
            lock.lock();
            try {
                if (onTrack) {
                    return false;
                } else {
                    onTrack = true;
                    return true;
                }
            } finally {
                lock.unlock();
            }
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

        private void acquire(Monitor m) throws CommandException, InterruptedException {
            tsi.setSpeed(this.id, 0);
            m.enter();
            tsi.setSpeed(this.id, this.speed);
        }

        private void acquireAndChangeSwitch(Monitor m,int x, int y, int direction) throws CommandException, InterruptedException {
            tsi.setSpeed(this.id, 0);
            m.enter();
            TSimInterface.getInstance().setSwitch(x, y, direction);
            tsi.setSpeed(this.id, this.speed);
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
                    leftSingleRail.leave();
                }
                return true;
            }

            if (x == 6 && y == 10) { // Currently on middle dual rail, bottom track.
                if (isDirectionDown()) {
                    acquireAndChangeSwitch(leftSingleRail, 4, 9, TSimInterface.SWITCH_RIGHT);
                } else {
                    leftSingleRail.leave();
                }
                return true;
            }

            if (x == 6 && y == 11) { // Top station (of the bottom ones)
                if (isDirectionDown()) {
                    leftSingleRail.leave();
                } else {
                    acquireAndChangeSwitch(leftSingleRail, 3, 11, TSimInterface.SWITCH_LEFT);
                }
                return true;
            }

            if (x == 5 && y == 13) { // Bottom station (of the bottom ones)
                if (isDirectionDown()) {
                    leftSingleRail.leave();
                } else {
                    acquireAndChangeSwitch(leftSingleRail, 3, 11, TSimInterface.SWITCH_RIGHT);
                    station2b.leave();
                }
                return true;
            }
            return false;
        }

        private void handleMiddleDualRail(int x, int y) throws CommandException {
            // Sensor on the right side of the dual rail in middle
            if (x == 19 && y == 8) {
                if (isDirectionDown()) { // Heading towards the dual rail
                    if (middleDualRail.tryEnter()) {
                        tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                        this.holdsMiddleDualRail = true;
                    } else {
                        tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    }
                } else { // Heading away from the dual rail and upwards
                    if (holdsMiddleDualRail) {
                        middleDualRail.leave();
                        holdsMiddleDualRail = false;
                    }

                    if (station1b.tryEnter()) {
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
                        middleDualRail.leave();
                        holdsMiddleDualRail = false;
                    }

                    if (station2b.tryEnter()) {
                        tsi.setSwitch(3, 11 ,TSimInterface.SWITCH_RIGHT);
                    } else {
                        tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                    }

                } else { // Heading towards the dual middle rail
                    if (middleDualRail.tryEnter()) { // Is someone on the bottom track?
                        tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                        this.holdsMiddleDualRail = true;
                    } else {
                        tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                    }
                }
            }
        }

        private boolean handleRightSingleRail(int x, int y) throws CommandException, InterruptedException {
            // To the right of the cross
            if (x == 14 && y == 7) { // Top of the two
                if (isDirectionDown()) {
                    acquireAndChangeSwitch(rightSingleRail,17,7,TSimInterface.SWITCH_RIGHT);
                } else {
                    rightSingleRail.leave();
                }
                return true;

            } else if (x == 14 && y == 8) { // Bottom of the two
                if (isDirectionDown()) {
                    acquireAndChangeSwitch(rightSingleRail,17,7,TSimInterface.SWITCH_LEFT);
                    station1b.leave();
                } else {
                    rightSingleRail.leave();
                }
                return true;
            }

            // You're currently on the middle dual rail. Top of the two
            if (x == 12 && y == 9) {
                if (isDirectionDown()) {
                    rightSingleRail.leave();
                } else {
                    acquireAndChangeSwitch(rightSingleRail,15,9,TSimInterface.SWITCH_RIGHT);
                }
                return true;
            }

            // You're currently on the middle dual rail. Bottom of the two
            if (x == 13 && y == 10) {
                if (this.direction == Direction.UP) {
                    acquireAndChangeSwitch(rightSingleRail,15,9,TSimInterface.SWITCH_LEFT);
                } else {
                    rightSingleRail.leave();
                }
                return true;
            }
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
            // You're west or north of the cross.
            if (x == 6 && y == 6 || x == 9 && y == 5) {
                if (isDirectionDown()) {
                    acquire(cross);
                } else {
                    cross.leave();
                }
                return true;
            }

            // You're east or south of the cross.
            if (x == 11 && y == 7 || x == 10 && y == 8) {
                if (isDirectionDown()) {
                    cross.leave();
                } else {
                    acquire(cross);
                }
                return true;
            }
            return false;
        }

        private boolean isSensorActive(SensorEvent e) {
            return e.getStatus() == 1;
        }
    }
}