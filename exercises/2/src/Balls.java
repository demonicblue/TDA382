import java.awt.Color;
import java.util.concurrent.Semaphore;
import javax.swing.*;

public class Balls {

    public static void nap(int ms) {
        try {
            Thread.sleep(ms);
        } catch (InterruptedException e) {
            //
            //  Print out the name of the tread that caused this.
            //
            System.err.println("Thread " + Thread.currentThread().getName() +
                    " throwed exception " + e.getMessage());
        }
    }

    public static void main(String[] a) {

        final BallWorld world = new BallWorld();
        final JFrame win = new JFrame();
        final Semaphore mutex = new Semaphore(1, true);
        try {
            mutex.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        Thread mutexThread = new Thread() {
            public void run() {
                nap(2000);
                for (int i = 0; i < 4; i++) {
                    nap((int) (5000 * Math.random()));
                    mutex.release();
                }
            }
        };

        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                win.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                win.getContentPane().add(world);
                win.pack();
                win.setVisible(true);
            }
        });

        Thread.currentThread().setName("MyMainThread");

        nap((int) (5000 * Math.random()));
        new Ball(world, 50, 80, 5, 10, Color.red, mutex).start();
        nap((int) (5000 * Math.random()));
        new Ball(world, 70, 100, 8, 6, Color.blue, mutex).start();
        nap((int) (5000 * Math.random()));
        new Ball(world, 150, 100, 9, 7, Color.green, mutex).start();
        nap((int) (5000 * Math.random()));
        new Ball(world, 200, 130, 3, 8, Color.black, mutex).start();
        nap((int) (5000 * Math.random()));
        mutexThread.start();
    }
}
