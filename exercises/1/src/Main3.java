import java.util.concurrent.*;

public class Main3 {
   private static void nap(int millisecs) {
        try {
            Thread.sleep(millisecs);
        } catch (InterruptedException e) {
            System.err.println(e.getMessage());
        }
    }

    private static void addProc(HighLevelDisplay d) {
        d.addRow("Bermuda is lol");
        nap(1500);
        d.addRow("Don't you like horses?");
        nap(3000);
        d.addRow("rawrisaurs");
        nap(3000);
        d.addRow("such wow. very doge.");
        nap(3000);
    }

    private static void deleteProc(HighLevelDisplay d) {
        d.deleteRow(0);
        nap(1500);
        d.deleteRow(0);
        nap(1500);
        d.deleteRow(0);
        nap(1500);


    }

    public static void main(String [] args) {
	final HighLevelDisplay d = new JDisplay2();
    final Semaphore mutex = new Semaphore(1, true);
	new Thread () {
	    public void run() {
        try {
            mutex.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
		addProc(d);
        mutex.release();
	    }
	}.start();


	new Thread () {
	    public void run() {
        try {
            mutex.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
		deleteProc(d);
        mutex.release();
	    }
	}.start();

    }
}