#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

vector<int> s;

void maxHeapify(vector<int>& A, int i) {
    int largest;

    int l = i*2;
    int r = i*2+1;
    if (l <= A.size()-1 && A[l] > A[i])
        largest = l;
    else
        largest = i;

    if (r <= A.size()-1 && A[r] > A[largest])
        largest = r;

    if (largest != i) {
        int tmp = A[i];
        A[i] = A[largest];
        A[largest] = tmp;
        maxHeapify(A, largest);
    }
}

int extract(vector<int>& A) {
    int max = A[1];
    A[1] = A[A.size()-1];
    A.erase(A.end()-1);
    maxHeapify(A, 1);

    return max;
}

void insertKey(vector<int>& A, int key) {
    A.push_back(-1);

    int i = A.size() - 1;
    A[i] = key;
    while (i > 1 && A[i/2] < A[i]) {
        int tmp = A[i];
        A[i] = A[i/2];
        A[i/2] = tmp;
        i = i/2;
    }
}

int main() {
    string op;
    s.push_back(0);
    while (true) {
        cin >> op;
        if (op == "insert") {
            int key;
            cin >> key;
            insertKey(s, key);
        } else if (op == "extract") {
            int max = extract(s);
            cout << max << endl;
        } else if (op == "end") {
            break;
        }
    }

    return 0;
}
