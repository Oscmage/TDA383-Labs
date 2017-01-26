import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.Semaphore;

public class Lab1 {
    private TSimInterface tsi;
    private Semaphore cross;
    private Semaphore rightSingleRail;
    private Semaphore leftSingleRail;
    private Semaphore middleDualRail;
    private boolean station1bFree = true;
    private boolean station2bFree = true;

    public Lab1(Integer speed1, Integer speed2) {
        tsi = TSimInterface.getInstance();

        cross = new Semaphore(1);
        rightSingleRail = new Semaphore(1);
        leftSingleRail = new Semaphore(1);
        middleDualRail = new Semaphore(1);

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


    private enum Direction {
        UP, DOWN
    }

    private class Train implements Runnable {
        private Direction direction;
        private int speed, id;

        public Train(Direction d, int originalSpeed, int id) {
            this.direction = d;
            this.speed = originalSpeed;
            this.id = id;
        }

        private void setSpeed(int speed) {
            try {
                tsi.setSpeed(this.id, speed);
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
                    handleEvent(e);

                } catch (CommandException e1) {
                    e1.printStackTrace();
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                }
            }

        }

        public void handleEvent(SensorEvent e) {
            if (isSensorActive(e)) {
                int x = e.getXpos();
                int y =  e.getYpos();
                if (handleStartStop(x, y)) {
                    return;
                } else if (handleCross(x, y)) {
                    return;
                } else if (handleRightSingleRail(x, y)) {
                    return;
                } else if (handleLeftSingleRail(x, y)) {
                    return;
                } else {
                    handleMiddelDualRail(x,y);
                }
            }
        }

        private boolean handleLeftSingleRail(int x, int y) {
            if (x == 7 && y == 9) {
                if (direction == Direction.DOWN) {
                    setSpeed(0);
                    acquire(leftSingleRail);
                    setSpeed(speed);
                    setSwitch(4,9,TSimInterface.SWITCH_LEFT);
                } else {
                    leftSingleRail.release();
                }
                return true;
            }

            if (x == 6 && y == 10) {
                if (direction == Direction.DOWN) {
                    setSpeed(0);
                    acquire(leftSingleRail);
                    setSwitch(4,9,TSimInterface.SWITCH_RIGHT);
                    setSpeed(speed);
                    middleDualRail.release();
                } else {
                    leftSingleRail.release();
                }
                return true;
            }

            if (x == 6 && y == 11) {
                if (direction == Direction.DOWN) {
                    leftSingleRail.release();
                } else {
                    setSpeed(0);
                    acquire(leftSingleRail);
                    setSwitch(3,11,TSimInterface.SWITCH_LEFT);
                    setSpeed(speed);
                }
                return true;
            }

            if (x == 5 && y == 13) {
                if (direction == Direction.DOWN) {
                    leftSingleRail.release();
                } else {
                    setSpeed(0);
                    acquire(leftSingleRail);
                    setSwitch(3,11,TSimInterface.SWITCH_RIGHT);
                    setSpeed(speed);
                    station2bFree = true;
                }
                return true;
            }
            return false;
        }

        private void handleMiddelDualRail(int x, int y) {

            //Handles right side
            if (x == 19 && y == 8) {
                if (direction == Direction.DOWN) {
                    if (middleDualRail.tryAcquire()) {
                        setSwitch(15,9,TSimInterface.SWITCH_LEFT);
                    } else {
                        setSwitch(15,9,TSimInterface.SWITCH_RIGHT);
                    }
                } else {
                    System.out.println("Station1bfree: " + station1bFree);
                    if (station1bFree) {
                        station1bFree = false;
                        setSwitch(17,7,TSimInterface.SWITCH_LEFT);
                    } else {
                        setSwitch(17,7,TSimInterface.SWITCH_RIGHT);
                    }
                }
            }

            //Handles left side
            if (x == 1 && y == 10) {
                if (direction == Direction.DOWN) {
                    if (station2bFree) {
                        setSwitch(3,11,TSimInterface.SWITCH_RIGHT);
                        station2bFree = false;
                    } else {
                        setSwitch(3,11,TSimInterface.SWITCH_LEFT);
                    }
                } else {
                    if (middleDualRail.tryAcquire()) {
                        setSwitch(4,9,TSimInterface.SWITCH_RIGHT);
                    } else {
                        setSwitch(4,9,TSimInterface.SWITCH_LEFT);
                    }
                }
            }
        }

        private boolean handleRightSingleRail(int x, int y) {
            //You're on the cross side of the single rail
            if (x == 14 && y == 7) {
                if (this.direction == Direction.DOWN) {
                    setSpeed(0);
                    acquire(rightSingleRail);
                    setSwitch(17,7,TSimInterface.SWITCH_RIGHT);
                    setSpeed(speed);
                } else {
                    rightSingleRail.release();
                }
                return true;
            } else if (x == 14 && y == 8) {
                if (this.direction == Direction.DOWN) {
                    setSpeed(0);
                    acquire(rightSingleRail);
                    setSwitch(17,7,TSimInterface.SWITCH_LEFT);
                    setSpeed(speed);
                    station1bFree = true;
                } else {
                    rightSingleRail.release();
                }
                return true;
            }

            //You're on the middle side of the right side single rail.
            if (x == 12 && y == 9) {
                if (this.direction == Direction.UP) {
                    setSpeed(0);
                    acquire(rightSingleRail);
                    setSwitch(15,9,TSimInterface.SWITCH_RIGHT);
                    setSpeed(speed);
                } else {
                    rightSingleRail.release();
                }
                return true;
            }

            if (x == 13 && y == 10) {
                if (this.direction == Direction.UP) {
                    setSpeed(0);
                    acquire(rightSingleRail);
                    setSwitch(15,9,TSimInterface.SWITCH_LEFT);
                    setSpeed(speed);
                    middleDualRail.release();
                } else {
                    rightSingleRail.release();
                }
                return true;
            }
            return false;
        }


        private void setSwitch(double x, double y, int direction) {
            try {
                tsi.setSwitch((int) x,(int) y,direction);
            } catch (CommandException e1) {
                e1.printStackTrace();
            }
        }

        private boolean handleStartStop(int x, int y) {
            if (x == 16 && (y == 3 || y == 5)) {
                if (this.direction == Direction.UP) slowDownAndSwitchDirection();
                return true;
            } else if (x == 15 && (y == 11 || y == 13)) {
                if (this.direction == Direction.DOWN) slowDownAndSwitchDirection();
                return true;
            }
            return false;
        }

        private void slowDownAndSwitchDirection() {
            setSpeed(0);
            sleep();
            this.speed = -this.speed;
            setSpeed(this.speed);
            if (this.direction == Direction.DOWN) this.direction = Direction.UP;
            else this.direction = Direction.DOWN;
        }

        private boolean handleCross(int x, int y) {

            if (x == 6 && y == 6 || x == 9 && y == 5) {
                if (this.direction == Direction.UP) {
                    cross.release();
                } else  {
                    setSpeed(0);
                    acquire(cross);
                    setSpeed(speed);
                }

                return true;
            }

            if (x == 11 && y == 7 || x == 10 && y == 8) {
                if (this.direction == Direction.DOWN) {
                    cross.release();
                }
                else {
                    setSpeed(0);
                    acquire(cross);
                    setSpeed(speed);
                }
                return true;
            }
            return false;
        }

        private void acquire(Semaphore s) {
            try {
                s.acquire();
            } catch (InterruptedException e1) {
                e1.printStackTrace();
            }
        }

        private boolean isSensorActive(SensorEvent e) {
            return e.getStatus() == 1;
        }

    }
}