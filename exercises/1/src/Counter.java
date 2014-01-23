class Counter implements Runnable {

    private int counter = 0;
    private final int rounds = 100000;

    public void run () {
        try {
            int id;
            String name = Thread.currentThread().getName();
            if (name.equals("thread1")) id = 1;
            else id = 2;
            for(int i = 0; i < rounds; i++) {
                if(id == 2 && i ==15000) {
                    Thread.sleep(200);
                }
                int tmp = counter;
                // Perhaps here?
                counter = tmp + 1;
                // Or here?
            }
        } catch (InterruptedException e) {
            System.out.println ("Interrupted!");
        }
    }

    public static void main (String[] args) {
        try {
            Counter c = new Counter ();

            // Create two threads that run our run () method.
            Thread t1 = new Thread (c, "thread1");
            Thread t2 = new Thread (c, "thread2");

            t1.start (); t2.start ();

            // Wait for the threads to finish.
            t1.join (); t2.join ();

            // Print the counter
            System.out.println(c.counter);

        } catch (InterruptedException e) {
            System.out.println ("Interrupted!");
        }
    }
}