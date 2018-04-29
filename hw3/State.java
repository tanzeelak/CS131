// The API for a simulation state. The only way to change the state is to invoke swap(i,j), where i and j are indexes into the array. If the ith entry in the array is positive and the jth entry is less than the maximum value allowed, the swap succeeds, subtracting 1 from the ith entry and adding 1 to the jth entry, returning true. Otherwise the swap fails and does nothing, returning false.

interface State {
    int size();
    byte[] current();
    boolean swap(int i, int j);
}
