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

        System.err.println("SUCH METHOD!");
        while (true) {
            SensorEvent event = tsim.getSensor(trainId);
            // Protect against dual events from same sensor
            if (event.getXpos() == lastXPos && event.getYpos() == lastYPos)
            {
                continue;
            }
            // Makes sure we pass a sensor without acting on it
            else if(event.getXpos() == blockXPos && event.getYpos() == blockYPos)
            {
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
                direction = DOWN;
                tsim.setSpeed(trainId, trainSpeed);

            } else if ( event.getXpos() == 14 && 
                        (event.getYpos() == 7 || event.getYpos() == 8) 
            ){ //Entering east block from north.
                if (direction == DOWN) {
                    System.err.println(trainId + ": Acquiring east");
                    tsim.setSpeed(trainId, 0);
                    east.acquire();
                    last = east;
                    tsim.setSpeed(trainId, trainSpeed);
                    if (event.getYpos() == 7) {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    } else {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                    }
                    
                } else if(direction == UP) {
                    System.err.println(trainId + ": Releasing east");
                    east.release();
                }
            } else if(event.getXpos() == 19 && event.getYpos() == 8) {
                //
                if (direction == UP) {
                    if (trainId == 1) {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    } else if(trainId == 2) {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                    }
                }
            } else if (event.getXpos() == 18 && event.getYpos() == 9) { //Entering midsection from north.
                if (direction == DOWN) {
                    if (dual.tryAcquire()) {
                        System.err.println(trainId + ": Acquired dual");
                        last = dual;
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    } else {
                        System.err.println(trainId + ": Dual already taken");
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    }
                    while(true) {
                        event = tsim.getSensor(trainId);
                        if (event.getStatus() == SensorEvent.INACTIVE && event.getXpos() == lastXPos && event.getYpos() == lastYPos)
                            break;
                    }
                } else if(direction == UP) { // UP
                    if(last == dual)
                        System.err.println(trainId + ": Releasing dual");
                        dual.release();
                        last = null;
                }
            } else if ( (event.getXpos() == 12 && event.getYpos() == 9 ) || 
                        (event.getXpos() == 12 && event.getYpos() == 10)
                ) {
                //Leaving east block
                if (direction == DOWN) {
                    System.err.println(trainId + ": Releasing east");
                    //event = tsim.getSensor(trainId);
                    east.release();
                } else {
                    //fixing up direction
                    System.err.println(trainId + ": Acquiring east");
                    tsim.setSpeed(trainId, 0);
                    east.acquire();
                    tsim.setSpeed(trainId, trainSpeed);
                    System.err.println("Acquired east");
                    if (event.getYpos() == 9) {
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                        System.err.println(trainId + ": Switiching to right");
                    } else {
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                        System.err.println(trainId + ": Switiching to left");
                    }

                }
            } else if ((event.getXpos() == 8 && event.getYpos() == 9) || (event.getXpos() == 8 && event.getYpos() == 10)) {
                if (direction == UP) { //Leaving west block
                    //event = tsim.getSensor(trainId); // WTF is this doing here?
                    System.err.println("Releasing west");
                    west.release();
                } else {
                    System.err.println("Acquiring west");
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
                System.err.println("dir: " + direction);
                if (direction == DOWN) { // Leaving dual track.
                    if (last == dual) {
                        System.err.println(trainId + ": Releasing dual");
                        dual.release();
                        last = null;
                    }
                    System.err.println("Loldafaq");
                    //passSensor(event);
                    //blockXPos = event.getXpos();
                    //blockYPos = event.getYpos();
                } else if(direction == UP) { // Entering dual track.
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
                
            } else if (event.getXpos() == 1 && event.getYpos() == 11) {
                // Entering south station
                if (direction == DOWN) {
                    if (trainId == 1) {
                        tsim.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                    } else if (trainId == 2) {
                        tsim.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                    }
                    //passSensor(event);
                    blockXPos = 1;
                    blockYPos = 9;
                }
            } else if (event.getXpos() == 6 && 
                (event.getYpos() == 11 || event.getYpos() == 13)
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
                if (event.getYpos() == 11) {
                    tsim.setSwitch(3, 11, TSimInterface.SWITCH_LEFT); 
                } else {
                    tsim.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                }  
            } else if (((event.getXpos() == 12 && event.getYpos() == 13))
                    ) { // Stop at the south station and return
                if(direction == UP)
                {
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
    /*
    *  Returns when the train has passed the last activated sensor, specified by event.
    */
    private void passSensor(SensorEvent event) throws CommandException, InterruptedException {
        int lastXPos = event.getXpos();
        int lastYPos = event.getYpos();
        while(true) {
            event = tsim.getSensor(trainId);
            
            if (event.getStatus() == SensorEvent.INACTIVE && 
                event.getXpos() == lastXPos && 
                event.getYpos() == lastYPos)
            {
                System.err.println(trainId + "Passed sensor");
                break;
            }
        }
    }

    private void east() throws CommandException, InterruptedException {
        if (direction == DOWN) {

        }
    }
}
