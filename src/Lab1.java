import TSim.*;

import java.util.concurrent.Semaphore;

public class Lab1 {
    private TSimInterface tsi;
    private Semaphore s1;
    private Direction t1;
    private Direction t2;

    public Lab1(Integer speed1, Integer speed2) {
        tsi = TSimInterface.getInstance();
        s1 = new Semaphore(1, true);

        try {
            tsi.setSpeed(1, speed1);
            tsi.setSpeed(2, speed2);
            t1 = Direction.DOWN;
            t2 = Direction.UP;
            listenEvent();
            tsi.setSpeed(1,0);
        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        }
    }

    private enum Direction {
        UP,DOWN
    }

    private void listenEvent() {
        while (true) {
            try {
                SensorEvent e = tsi.getSensor(1);
                handleEvent(e);
            } catch (CommandException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    private void handleEvent(SensorEvent e) {
        int x = e.getXpos();
        int y = e.getXpos();
        if (x == 15 && y == 13) {
            try {
                tsi.setSpeed(e.getTrainId(),0);
                t1 = Direction.UP;
                tsi.setSpeed(e.getTrainId(),-15);
            } catch (CommandException e1) {
                e1.printStackTrace();
            }
        }
    }


}
