import java.util.concurrent.ThreadLocalRandom;

//A Runnable class that tests a state implementation by performing a given number of successful swaps on it. It does not count failed swaps.
class SwapTest implements Runnable {
    private int nTransitions;
    private State state;

    SwapTest(int n, State s) {
	nTransitions = n;
	state = s;
    }

    public void run() {
	int n = state.size();
	if (n != 0)
	    for (int i = 0; i < nTransitions; ) {
		int a = ThreadLocalRandom.current().nextInt(0, n);
		int b = ThreadLocalRandom.current().nextInt(0, n - 1);
		if (a == b)
		    b = n - 1;
		if (state.swap(a, b))
		    i++;
	    }
    }
}
