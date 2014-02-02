import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.Semaphore;

public class Lab1 extends Thread {
    public static final int UP = 1;
    public static final int DOWN = 2;
    public static final int STATION_WAIT = 1500;
    private int direction;

    public static Semaphore east;
    public static Semaphore west;
    public static Semaphore crossing;
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
        Lab1.crossing = new Semaphore(1, true);
        train1.start();
        train2.start();

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
        int blockXPos = 0, blockYPos = 0;

        while (true) {
            SensorEvent event = tsim.getSensor(trainId);
            int xPos = event.getXpos();
            int yPos = event.getYpos();

            // Protect against dual events from same sensor
            if (xPos == lastXPos && yPos == lastYPos) {
                continue;
            }
            // Makes sure we pass a sensor without acting on it
            else if (xPos == blockXPos && yPos == blockYPos) {
                continue;
            } else {
                lastXPos = xPos;
                lastYPos = yPos;
            }
            if (((xPos == 12 && yPos == 3)
                    || (xPos == 12 && yPos == 5))
                    && direction == UP) {
                tsim.setSpeed(trainId, 0);
                Thread.sleep(STATION_WAIT + 2 * simSpeed * Math.abs(trainSpeed));
                trainSpeed = (-1) * trainSpeed;
                direction = DOWN;
                tsim.setSpeed(trainId, trainSpeed);

            } else if ((xPos == 6 && yPos == 6) || (xPos == 9 && yPos == 5)) { //Entering crossing from north station.
                if (direction == DOWN) {
                    tsim.setSpeed(trainId, 0);
                    crossing.acquire();
                    last = crossing;
                    tsim.setSpeed(trainId, trainSpeed);
                } else if (direction == UP) { //Leaving crossing in north direction.
                    if (last == crossing) {
                        crossing.release();
                        last = null;
                    }
                }
            } else if (xPos == 11 && (yPos == 7 || yPos == 8)) {
                if (direction == UP) { //Entering crossing from south direction.
                    tsim.setSpeed(trainId, 0);
                    crossing.acquire();
                    last = crossing;
                    tsim.setSpeed(trainId, trainSpeed);
                } else if (direction == DOWN) { //Leaving crossing in south direction.
                    crossing.release();
                    last = null;
                }
            } else if (xPos == 14 &&
                    (yPos == 7 || yPos == 8)
                    ) { //Entering east block from north.
                if (direction == DOWN) {
                    System.err.println(trainId + ": Acquiring east");
                    tsim.setSpeed(trainId, 0);
                    east.acquire();
                    last = east;
                    tsim.setSpeed(trainId, trainSpeed);
                    if (yPos == 7) {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    } else {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                    }

                } else if (direction == UP) {
                    System.err.println(trainId + ": Releasing east");
                    east.release();
                }
            } else if (xPos == 19 && yPos == 8) {
                //
                if (direction == UP) {
                    if (trainId == 1) {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    } else if (trainId == 2) {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                    }
                }
            } else if (xPos == 18 && yPos == 9) { //Entering midsection from north.
                if (direction == DOWN) {
                    if (dual.tryAcquire()) {
                        System.err.println(trainId + ": Acquired dual");
                        last = dual;
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    } else {
                        System.err.println(trainId + ": Dual already taken");
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    }
                    while (true) {
                        event = tsim.getSensor(trainId);
                        if (event.getStatus() == SensorEvent.INACTIVE && xPos == lastXPos && yPos == lastYPos)
                            break;
                    }
                } else if (direction == UP) { // UP
                    if (last == dual) {
                        System.err.println(trainId + ": Releasing dual");
                        dual.release();
                        last = null;
                    }
                }
            } else if ((xPos == 12 && yPos == 9) ||
                    (xPos == 12 && yPos == 10)
                    ) {
                //Leaving east block
                if (direction == DOWN) {
                    System.err.println(trainId + ": Releasing east");
                    east.release();
                } else {
                    //fixing up direction
                    System.err.println(trainId + ": Acquiring east");
                    tsim.setSpeed(trainId, 0);
                    east.acquire();
                    tsim.setSpeed(trainId, trainSpeed);
                    System.err.println(trainId + ": Acquired east");
                    if (yPos == 9) {
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                        System.err.println(trainId + ": Switiching to right");
                    } else {
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                        System.err.println(trainId + ": Switiching to left");
                    }

                }
            } else if ((xPos == 8 && yPos == 9) || (xPos == 8 && yPos == 10)) {
                blockXPos = 0;
                blockYPos = 0;

                if (direction == UP) { //Leaving west block
                    //event = tsim.getSensor(trainId); // WTF is this doing here?
                    System.err.println(trainId + ": Releasing west");
                    west.release();
                } else {
                    System.err.println(trainId + ": Acquiring west");
                    tsim.setSpeed(trainId, 0);
                    west.acquire();
                    tsim.setSpeed(trainId, trainSpeed);
                    if (yPos == 9) {
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                    } else {
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                    }
                }
            } else if (xPos == 1 && yPos == 9) {
                System.err.println("dir: " + direction);
                if (direction == DOWN) { // Leaving dual track.
                    if (last == dual) {
                        System.err.println(trainId + ": Releasing dual");
                        dual.release();
                        last = null;
                    }
                    System.err.println("Loldafaq");
                } else if (direction == UP) { // Entering dual track.
                    if (dual.tryAcquire()) {
                        last = dual;
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                        System.err.println(trainId + ": Acquired dual track");
                    } else {
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                        System.err.println(trainId + ": Failed to acquire dual track");
                    }
                    blockXPos = 1;
                    blockYPos = 11;
                }

            } else if (xPos == 1 && yPos == 11) {
                // Entering south station
                if (direction == DOWN) {
                    if (trainId == 1) {
                        System.err.println(trainId + ": to lower track");
                        tsim.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                    } else if (trainId == 2) {
                        System.err.println(trainId + ": to upper track");
                        tsim.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                    }
                    //passSensor(event);
                    blockXPos = 1;
                    blockYPos = 9;
                }
            } else if (xPos == 6 &&
                    (yPos == 11 || yPos == 13)
                    ) {
                if (direction == DOWN) {
                    System.err.println(trainId + ": Releasing west");
                    west.release();
                    blockXPos = 0;
                    blockYPos = 0;
                    continue;
                }
                System.err.println(trainId + ": Acquiring west");
                tsim.setSpeed(trainId, 0);
                west.acquire();
                tsim.setSpeed(trainId, trainSpeed);
                if (yPos == 11) {
                    tsim.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                } else {
                    tsim.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                }
            } else if (xPos == 12 &&
                    (yPos == 13 || yPos == 11)
                    ) { // Stop at the south station and return
                if (direction == UP) {
                    System.err.println("Skipping");
                    continue;
                }
                System.err.println(trainId + ": Stopping at station");
                tsim.setSpeed(trainId, 0);
                Thread.sleep(STATION_WAIT + 2 * simSpeed * Math.abs(trainSpeed));
                trainSpeed = (-1) * trainSpeed;
                direction = UP;
                tsim.setSpeed(trainId, trainSpeed);
            }

        } // end of while true

    }
}
