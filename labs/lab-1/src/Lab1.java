import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.Semaphore;

public class Lab1 extends Thread {
    public static Semaphore east;
    private int trainId;

    public static void main(String[] args) {
        Lab1 train1 = new Lab1(args, 1);
        Lab1 train2 = new Lab1(args, 2);

        Lab1.east = new Semaphore(1, true);
        train1.start();
        train2.start();

    }

    public Lab1(String[] args, int id) {
        trainId = id;
    }

    public void run() {
        TSimInterface tsi = TSimInterface.getInstance();
        try {
            tsi.setSpeed(1, 100);
            while (true) {
                SensorEvent event = tsi.getSensor(trainId);
                if (event.getXpos() == 15 && event.getYpos() == 7) {
                    tsi.setSpeed(trainId, 0);
                    east.acquire();
                    tsi.setSpeed(trainId, 10);
                }
            }

        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
