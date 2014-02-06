import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import java.util.HashSet;
import java.util.Set;

public class Lab2 extends Thread {
    public static final int UP = 1;
    public static final int DOWN = 2;
    public static final int STATION_WAIT = 1500;
    private int direction;

    private static TrackMonitor east = new TrackMonitor("east");
    private static TrackMonitor west = new TrackMonitor("west");
    private static TrackMonitor crossing = new TrackMonitor("crossing");
    private static TrackMonitor dual = new TrackMonitor("dual");
    private static TrackMonitor northStation = new TrackMonitor("northStation");
    private static TrackMonitor southStation = new TrackMonitor("southStation");

    private int trainId;
    private int trainSpeed;

    private static int simSpeed;

    private static TSimInterface tsim;
    //private TrackMonitor last;

    private HashSet<TrackMonitor> inventory = new HashSet<TrackMonitor>();

    public static void main(String[] args) {
        if (args.length < 3) {
            simSpeed = 100;
        } else {
            simSpeed = Integer.parseInt(args[2]);
        }

        tsim = TSimInterface.getInstance();

        Lab2 train1 = new Lab2(1, Integer.parseInt(args[0]), DOWN);
        Lab2 train2 = new Lab2(2, Integer.parseInt(args[1]), UP);

        train1.start();
        train2.start();

    }

    public Lab2(int id, int speed, int direction) {
        trainId = id;
        trainSpeed = speed;
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
                stationStopAndChangeDirection(DOWN);

            } else if ((xPos == 6 && yPos == 6) || (xPos == 9 && yPos == 5)) { //Entering crossing from north station.
                if (direction == DOWN) {
                    tsim.setSpeed(trainId, 0);
                    crossing.enter();
                    //last = crossing;
                    inventory.add(crossing);
                    tsim.setSpeed(trainId, trainSpeed);
                } else if (direction == UP) { //Leaving crossing in north direction.
                    //if (last == crossing) {
                    if (inventory.contains(crossing)) {
                        crossing.leave();
                        //last = null;
                        inventory.remove(crossing);
                    }
                }
            } else if (xPos == 11 && (yPos == 7 || yPos == 8)) {
                if (direction == UP) { //Entering crossing from south direction.
                    tsim.setSpeed(trainId, 0);
                    crossing.enter();
                    //last = crossing;
                    inventory.add(crossing);
                    tsim.setSpeed(trainId, trainSpeed);
                } else if (direction == DOWN) { //Leaving crossing in south direction.
                    crossing.leave();
                    //last = null;
                    inventory.remove(crossing);
                }
            } else if (xPos == 14 &&
                    (yPos == 7 || yPos == 8)
                    ) { //Entering east block from north.
                if (direction == DOWN) {
                    tsim.setSpeed(trainId, 0);
                    east.enter();
                    //last = east;
                    inventory.add(east);
                    tsim.setSpeed(trainId, trainSpeed);
                    if (yPos == 7) {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    } else {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                    }

                } else if (direction == UP) {
                    east.leave();
                    blockXPos = 0;
                    blockYPos = 0;
                }
            } else if (xPos == 19 && yPos == 8) {
                // Enter north station
                if (direction == UP) {
                    blockXPos = 18;
                    blockYPos = 9;

                    if( northStation.tryEnter() ) {
                        //last = northStation;
                        inventory.add(northStation);
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                        System.err.println(trainId + ": Did get station");
                    } else {
                        tsim.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                        System.err.println(trainId + ": Didn't get station");
                    }
                } else if(direction == DOWN) {
                    //if (last == northStation) {
                    if (inventory.contains(northStation)) {
                        northStation.leave();
                        //last = null;
                        inventory.remove(northStation);
                        System.err.println(trainId + ": Released station");
                    }
                }
            } else if (xPos == 18 && yPos == 9) { //Entering midsection from north.
                if (direction == DOWN) {
                    blockXPos = 19;
                    blockYPos = 8;

                    if (dual.tryEnter()) {
                        //last = dual;
                        inventory.add(dual);
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    } else {
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    }
                    while (true) {
                        event = tsim.getSensor(trainId);
                        if (event.getStatus() == SensorEvent.INACTIVE && xPos == lastXPos && yPos == lastYPos)
                            break;
                    }
                } else if (direction == UP) { // UP
                    //if (last == dual) {
                    if (inventory.contains(dual)) {
                        dual.leave();
                        //last = null;
                        inventory.remove(dual);
                    }

                }
            } else if ((xPos == 12 && yPos == 9) ||
                    (xPos == 12 && yPos == 10)
                    ) {
                //Leaving east block
                if (direction == DOWN) {
                    east.leave();
                    blockXPos = 0;
                    blockYPos = 0;
                } else {
                    //fixing up direction
                    tsim.setSpeed(trainId, 0);
                    east.enter();
                    tsim.setSpeed(trainId, trainSpeed);
                    if (yPos == 9) {
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    } else {
                        tsim.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    }

                }
            } else if ((xPos == 8 && yPos == 9) || (xPos == 8 && yPos == 10)) {
                blockXPos = 0;
                blockYPos = 0;

                if (direction == UP) { //Leaving west block
                    west.leave();
                } else {
                    tsim.setSpeed(trainId, 0);
                    west.enter();
                    tsim.setSpeed(trainId, trainSpeed);
                    if (yPos == 9) {
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                    } else {
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                    }
                }
            } else if (xPos == 1 && yPos == 9) {
                if (direction == DOWN) { // Leaving dual track.
                    //if (last == dual) {
                    if(inventory.contains(dual)) {
                        dual.leave();
                        //last = null;
                        inventory.remove(dual);
                    }
                } else if (direction == UP) { // Entering dual track.
                    if (dual.tryEnter()) {
                        //last = dual;
                        inventory.add(dual);
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                    } else {
                        tsim.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                    }
                    blockXPos = 1;
                    blockYPos = 11;
                }

            } else if (xPos == 1 && yPos == 11) {
                // Entering south station
                if (direction == DOWN) {
                    if ( southStation.tryEnter() ) {
                        tsim.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                        //last = southStation;
                        inventory.add(southStation);
                    } else {
                        tsim.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                    }
                    blockXPos = 1;
                    blockYPos = 9;
                } else if (direction == UP) {
                    //if (last == southStation) {
                    if (inventory.contains(southStation)) {
                        southStation.leave();
                        //last = null;
                        inventory.remove(southStation);
                    }
                }
            } else if (xPos == 6 &&
                    (yPos == 11 || yPos == 13)
                    ) {
                if (direction == DOWN) {
                    west.leave();
                    blockXPos = 0;
                    blockYPos = 0;
                    continue;
                }
                tsim.setSpeed(trainId, 0);
                west.enter();
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
                    continue;
                }
                stationStopAndChangeDirection(UP);
            }

        } // end of while true

    }

    private void stationStopAndChangeDirection(int direction) throws CommandException, InterruptedException {
        tsim.setSpeed(trainId, 0);
        Thread.sleep(STATION_WAIT + 2 * simSpeed * Math.abs(trainSpeed));
        trainSpeed = (-1) * trainSpeed;
        this.direction = direction;
        tsim.setSpeed(trainId, trainSpeed);
    }


    public static class TrackMonitor {

        private Lock lock;

        private boolean onTrack;
        private Condition notOnTrack;
        private String name;

        public TrackMonitor(String name) {
            this.name = name;
            lock = new ReentrantLock();
            notOnTrack = lock.newCondition();
            onTrack = false;

        }

        private void enter() throws InterruptedException {
            lock.lock();
            //System.err.println("Trying to enter track: " + name);
            while (onTrack) notOnTrack.await();

            onTrack = true;
            //System.err.println("Enter track: " + name);
            lock.unlock();
        }

        private void leave() {
            lock.lock();
            //System.err.println("Trying to leave track: " + name);
            onTrack = false;
            notOnTrack.signal();
            //System.err.println("Left track: " + name);
            lock.unlock();
        }

        private boolean tryEnter() {
            lock.lock();
            //System.err.println("Trying to enter dual track: " + name);
            if (onTrack) {
                //System.err.println("Switching track");
                lock.unlock();
                return false;
            }
            
            onTrack = true;
            //System.err.println("Got dual track: " + name);
            lock.unlock();
            return true;
        }
    }
}
