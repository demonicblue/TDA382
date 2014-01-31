import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.Semaphore;

public class Lab1 extends Thread {
    public static Semaphore east;
    private int trainId;

    private int trainSpeed;
    private static int simSpeed;

    private TSimInterface tsim;

    public static void main(String[] args) {
        if (args.length < 3) {
            simSpeed = 100;
        } else {
            simSpeed = Integer.parseInt(args[2]);
        }

        Lab1 train1 = new Lab1(1, Integer.parseInt(args[0]));
        Lab1 train2 = new Lab1(2, Integer.parseInt(args[1]));

        Lab1.east = new Semaphore(1, true);
        train1.start();
        train2.start();

    }

    public Lab1(int id, int speed) {
        trainId = id;
        trainSpeed = speed;
        tsim = TSimInterface.getInstance();
    }

    public void run() {
        try {
            tsim.setSpeed(trainId, trainSpeed);
            while(true){
                logic();
            }

        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private void logic() throws CommandException, InterruptedException {
        System.err.println("SUCH METHOD!");
        SensorEvent event = tsim.getSensor(trainId);
        if(event.getXpos() == 16 && event.getYpos() == 7)
        {
            System.err.println("First sensor?");
            tsim.setSpeed(trainId, 0);
            east.acquire();
            tsim.setSpeed(trainId, 100);
            tsim.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
            System.err.println("Passing first sensor");
            while(event.getStatus()!=event.INACTIVE) {
                event = tsim.getSensor(trainId);
            }
            System.err.println("Exited first sensor");
        }
    }
}
