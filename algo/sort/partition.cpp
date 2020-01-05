#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

vector<int> S;

int partition(vector<int>& A, int p, int r) {
    int x = A[r];
    int i = p - 1;
    for (int j=p; j<r; j++) {
        if (A[j] <= x) {
            i++;
            int temp = A[i];
            A[i] = A[j];
            A[j] = temp;
        }
    }
    int temp = A[i+1];
    A[i+1] = A[r];
    A[r] = temp;
    return i+1;
}

int main() {
    int n;
    cin >> n;
    int i;
    rep(i, n) {
        int input;
        cin >> input;
        S.push_back(input);
    }

    int idx = partition(S, 0, n-1);
    rep(i, n-1) {
        if (i == idx) {
            cout << "[" << S[i] << "] ";
        } else {
            cout << S[i] << " ";
        }
    }
    cout << S[n-1] << endl;

    return 0;
}
