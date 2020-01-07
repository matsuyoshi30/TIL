#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

vector<int> S;

void countingSort(vector<int>& A, vector<int>& B, int l, int k) {
    vector<int> C(k+1, 0);

    for (int j=0; j<l; j++)
        C[A[j]]++;

    for (int i=1; i<=k; i++)
        C[i] = C[i] + C[i-1];

    for (int j=l-1; j>=0; j--) {
        B[C[A[j]] - 1] = A[j];
        C[A[j]]--;
    }
}

int main() {
    int n;
    cin >> n;
    int i;
    int max = -1;
    rep(i, n) {
        int input;
        cin >> input;
        S.push_back(input);
        if (max < input) max = input;
    }
    vector<int> T(n);
    countingSort(S, T, n, max);

    rep(i, n-1) {
        cout << T[i] << " ";
    }
    cout << T[n-1] << endl;

    return 0;
}
