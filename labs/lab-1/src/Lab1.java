import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.Semaphore;

public class Lab1 extends Thread {
    public static final int UP = 1;
    public static final int DOWN = 2;

    private int direction;

    public static Semaphore east;
    public static Semaphore intersection;
    public static Semaphore dual;

    private int trainId;
    private int trainSpeed;
    private static int simSpeed;

    private TSimInterface tsim;

    private Semaphore last;

    public static void main(String[] args) {
        if (args.length < 3) {
            simSpeed = 100;
        } else {
            simSpeed = Integer.parseInt(args[2]);
        }

        Lab1 train1 = new Lab1(1, Integer.parseInt(args[0]), DOWN);
        Lab1 train2 = new Lab1(2, Integer.parseInt(args[1]), UP);

        Lab1.east = new Semaphore(1, true);
        train1.start();
        //train2.start();

    }

    public Lab1(int id, int speed, int direction) {
        trainId = id;
        trainSpeed = speed;
        tsim = TSimInterface.getInstance();
        this.direction = direction;
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
        while(true) {
            SensorEvent event = tsim.getSensor(trainId);
            if(event.getXpos() == 16 && event.getYpos() == 7) {
                if(direction == DOWN) {
                    System.err.println("First sensor?");
                    tsim.setSpeed(trainId, 0);
                    east.acquire();
                    last = east;
                    tsim.setSpeed(trainId, 100);
                    tsim.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    System.err.println("Passing first sensor");
                }
            } else if (event.getXpos() == 16 && event.getYpos() == 9) {
                if (direction == DOWN) {
                    if(dual.tryAcquire()) {
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    } else {
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    }
                }
            } else if(event.getXpos() == 14 && event.getYpos() == 9) {
                if(direction == DOWN) {
                    while(event.getStatus() != SensorEvent.INACTIVE) {
                        event = tsim.getSensor(trainId);
                    }
                }
            }
        } // end of while true

    }

    private void east() throws CommandException, InterruptedException {
        if(direction == DOWN) {

        }
    }
}
