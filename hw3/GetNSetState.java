import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private byte[] value;
    private AtomicIntegerArray valueAtomic;
    private byte maxval;

    private AtomicIntegerArray copyToAtomicArray(byte[] v) {
	int vSize = v.length;
	int[] array = new int[vSize];
	for (int i = 0; i < vSize; i++){
	    array[i] = v[i];
	}
	valueAtomic = new AtomicIntegerArray(array);
	return valueAtomic;
    }

    private byte[] copyToByteArray(){
	int aSize = valueAtomic.length();
	byte[] array = new byte[aSize];
	for (int i = 0; i < aSize; i++) {
	    array[i] = (byte) valueAtomic.get(i);
	}
	return array;
    }
    
    GetNSetState(byte[] v) {
	copyToAtomicArray(v);
	maxval = 127;
    }

    GetNSetState(byte[] v, byte m) {
	copyToAtomicArray(v);
	maxval = m;
    }

    public int size() {
	return valueAtomic.length();
    }

    public byte[] current() {
	return copyToByteArray();
    }

    public boolean swap(int i, int j) {
	if (valueAtomic.get(i) <= 0 || valueAtomic.get(j) >= maxval) {
	    return false;
	}
	valueAtomic.decrementAndGet(i);
	valueAtomic.incrementAndGet(j);
	return true;
    }
}
