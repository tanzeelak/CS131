// A test harness, with a main method. Invoke it via a shell command like "java UnsafeMemory Synchronized 8 1000000 6 5 6 3 0 3". Here, Synchronized means to test the SynchronizedState implementation; 8 means to divide the work into 8 threads of roughly equal size; 1000000 means to do a million successful swap transitions total; 6 is maxval, an integer in the range [0,127] as described above; and the remaining five numbers are the initial values for the five entries in the state array. The shell command outputs a string like "Threads average 3318.01 ns/transition", giving the approximate average number of real-time nanoseconds that it took a thread to do a successful swap, including all the overhead. It also outputs an error diagnostic if a reliability test fails.
class UnsafeMemory {
    public static void main(String args[]) {
	if (args.length < 3)
	    usage(null);
	try {
	    int nThreads = parseInt (args[1], 1, Integer.MAX_VALUE);
	    int nTransitions = parseInt (args[2], 0, Integer.MAX_VALUE);
	    byte maxval = (byte) parseInt (args[3], 0, 127);
	    byte[] value = new byte[args.length - 4];
	    for (int i = 4; i < args.length; i++)
		value[i - 4] = (byte) parseInt (args[i], 0, maxval);
	    byte[] stateArg = value.clone();
	    State s;
	    if (args[0].equals("Null"))
		s = new NullState(stateArg, maxval);
	    else if (args[0].equals("Synchronized"))
		s = new SynchronizedState(stateArg, maxval);
	    else if (args[0].equals("Unsynchronized"))
                s = new UnsynchronizedState(stateArg, maxval);
            else if (args[0].equals("GetNSet"))
                s = new GetNSet(stateArg, maxval);
	    else
		throw new Exception(args[0]);
	    dowork(nThreads, nTransitions, s);
	    test(value, s.current(), maxval);
	    System.exit (0);
	} catch (Exception e) {
	    usage(e);
	}
    }

    private static void usage(Exception e) {
	if (e != null)
	    System.err.println(e);
	System.err.println("Usage: model nthreads ntransitions"
			   + " maxval n0 n1 ...\n");
	System.exit (1);
    }

    private static int parseInt(String s, int min, int max) {
	int n = Integer.parseInt(s);
	if (n < min || n > max)
	    throw new NumberFormatException(s);
	return n;
    }

    private static void dowork(int nThreads, int nTransitions, State s)
      throws InterruptedException {
	Thread[] t = new Thread[nThreads];
	for (int i = 0; i < nThreads; i++) {
	    int threadTransitions =
		(nTransitions / nThreads
		 + (i < nTransitions % nThreads ? 1 : 0));
	    t[i] = new Thread (new SwapTest (threadTransitions, s));
	}
	long start = System.nanoTime();
	for (int i = 0; i < nThreads; i++)
	    t[i].start ();
	for (int i = 0; i < nThreads; i++)
	    t[i].join ();
	long end = System.nanoTime();
	double elapsed_ns = end - start;
	System.out.format("Threads average %g ns/transition\n",
			  elapsed_ns * nThreads / nTransitions);
    }

    private static void test(byte[] input, byte[] output, byte maxval) {
	if (input.length != output.length)
	    error("length mismatch", input.length, output.length);
	long isum = 0;
	long osum = 0;
	for (int i = 0; i < input.length; i++)
	    {
		isum += input[i];
		osum += output[i];
		if (output[i] < 0)
		    error("negative output", output[i], 0);
		if (output[i] > maxval)
		    error("output too large", output[i], maxval);
	    }
	if (isum != osum)
	    error("sum mismatch", isum, osum);
    }

    private static void error(String s, long i, long j) {
	System.err.format("%s (%d != %d)\n", s, i, j);
	System.exit(1);
    }
}
