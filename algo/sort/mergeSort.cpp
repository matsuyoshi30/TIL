#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

vector<long long> S;
int total;

void merge(vector<long long>& A, int left, int mid, int right) {
    int n1 = mid - left;
    int n2 = right - mid;
    vector<long long> L(n1+1);
    vector<long long> R(n2+1);
    int i;
    rep(i, n1) {
        L[i] = A[left + i];
    }
    rep(i, n2) {
        R[i] = A[mid + i];
    }
    L[n1] = 1000000001;
    R[n2] = 1000000001;
    int x = 0, y = 0;
    for (int k = left; k < right; k++) {
        if (L[x] <= R[y]) {
            A[k] = L[x];
            x++;
            total++;
        } else {
            A[k] = R[y];
            y++;
            total++;
        }
    }
}

void mergeSort(vector<long long>& A, int left, int right) {
    if (left + 1 < right) {
        int mid = (left + right) / 2;
        mergeSort(A, left, mid);
        mergeSort(A, mid, right);
        merge(A, left, mid, right);
    }
}

int main() {
    int n;
    cin >> n;
    int i;
    rep(i, n) {
        long long input;
        cin >> input;
        S.push_back(input);
    }
    mergeSort(S, 0, n);
    rep(i, n-1) {
        cout << S[i] << " ";
    }
    cout << S[n-1] << endl;
    cout << total << endl;

    return 0;
}
