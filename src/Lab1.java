import TSim.*;

public class Lab1 {

    public Lab1(Integer speed1, Integer speed2) {
        TSimInterface tsi = TSimInterface.getInstance();

        try {
            tsi.setSpeed(1, speed1);
            tsi.setSpeed(2, speed2);
            System.out.println("Sensor XPOS: " + tsi.getSensor(1).getXpos());
            tsi.setSpeed(1,0);
        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }


}
