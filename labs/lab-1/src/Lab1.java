import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.Semaphore;

public class Lab1 extends Thread {
    public static final int UP = 1;
    public static final int DOWN = 2;
    public static final int stationWait = 1500;
    public static final int STATION_WAIT = stationWait;

    private int direction;

    public static Semaphore east;
    public static Semaphore west;
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
        Lab1.dual = new Semaphore(1, true);
        Lab1.west = new Semaphore(1, true);
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

            logic();


        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private void logic() throws CommandException, InterruptedException {
        int lastXPos = 0, lastYPos = 0;

        System.err.println("SUCH METHOD!");
        while (true) {
            SensorEvent event = tsim.getSensor(trainId);
            if (event.getXpos() == lastXPos && event.getYpos() == lastYPos) {
                continue;
            }
            else {
                lastXPos = event.getXpos();
                lastYPos = event.getYpos();
            }
            if (((event.getXpos() == 12 && event.getYpos() == 3)
                    || (event.getXpos() == 12 && event.getYpos() == 5))
                    && direction == UP) {
                tsim.setSpeed(trainId, 0);
                Thread.sleep(STATION_WAIT + 2 * simSpeed * Math.abs(trainSpeed));
                trainSpeed = (-1) * trainSpeed;
                tsim.setSpeed(trainId, trainSpeed);

            } else if (event.getXpos() == 14 && event.getYpos() == 7) { //Entering east block from north.
                if (direction == DOWN) {
                    System.err.println("First sensor?");
                    tsim.setSpeed(trainId, 0);
                    east.acquire();
                    last = east;
                    tsim.setSpeed(trainId, 100);
                    tsim.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    System.err.println("Passing first sensor");
                }
            } else if(event.getXpos() == 19 && event.getYpos() == 8) {
                System.err.println("LOLOL:");
            } else if (event.getXpos() == 18 && event.getYpos() == 9) { //Entering midsection from north.
                if (direction == DOWN) {
                    if (dual.tryAcquire()) {
                        System.err.println("Acquired dual");
                        last = dual;
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    } else {
                        System.err.println("Dual already taken");
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    }
                    while(true) {
                        event = tsim.getSensor(trainId);
                        if (event.getStatus() == SensorEvent.INACTIVE && event.getXpos() == lastXPos && event.getYpos() == lastYPos)
                            break;
                    }
                }
            } else if ((event.getXpos() == 12 && event.getYpos() == 9) || (event.getXpos() == 12 && event.getYpos() == 10)) {
                //Leaving east block
                if (direction == DOWN) {
                    event = tsim.getSensor(trainId);
                    east.release();
                } else {
                    //fixing up direction
                }
            } else if ((event.getXpos() == 8 && event.getYpos() == 9) || (event.getXpos() == 8 && event.getYpos() == 10)) {
                //Leaving west block
                if (direction == UP) {
                    event = tsim.getSensor(trainId);
                    west.release();
                } else {
                    tsim.setSpeed(trainId, 0);
                    west.acquire();
                    tsim.setSpeed(trainId, trainSpeed);
                    if (event.getYpos() == 9) {
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                    } else {
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                    }
                }
            } else if (event.getXpos() == 1 && event.getYpos() == 9) {
                if (direction == DOWN && last == dual) { // Leaving dual track.
                    dual.release();
                } else { // Entering dual track.
                    if (dual.tryAcquire()) {
                        last = dual;
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                    } else {
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                    }
                }
            } else if (event.getXpos() == 1 && event.getYpos() == 11) {
                if (direction == DOWN) {
                    if (trainId == 1) {
                        tsim.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                    } else if (trainId == 2) {
                        tsim.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                    }
                }
            } else if (((event.getXpos() == 12 && event.getYpos() == 11)
                    || (event.getXpos() == 12 && event.getYpos() == 14))
                    && direction == DOWN) {
                tsim.setSpeed(trainId, 0);
                Thread.sleep(STATION_WAIT + 2 * simSpeed * Math.abs(trainSpeed));
                trainSpeed = (-1) * trainSpeed;
                tsim.setSpeed(trainId, trainSpeed);

            }

        } // end of while true

    }

    private void east() throws CommandException, InterruptedException {
        if (direction == DOWN) {

        }
    }
}
