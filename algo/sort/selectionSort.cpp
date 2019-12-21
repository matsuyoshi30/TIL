#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

/*
 * selection sort
 *  未ソート部分の先頭の要素を、未ソート部分の最小要素と交換
 *  全体の要素の先頭からソート済の列になる
 *
 *  安定ではない
 */
int selectionSort(vector<int>& a, int n) {
    int min_j, cnt = 0;
    for (int i=0; i<n; i++) {
        min_j = i;
        for (int j=i; j<n; j++) {
            if (a[j] < a[min_j]) {
                min_j = j;
            }
        }
        swap(a[i], a[min_j]);
        if (min_j != i) cnt++;
    }

    for (int i=0; i<a.size()-1; i++) cout << a[i] << " ";
    cout << a[n-1] << endl;

    return cnt;
}

int main() {
    int N;
    cin >> N;
    vector<int> A(N);
    for (int i=0; i<N; i++) cin >> A[i];

    cout << selectionSort(A, N) << endl;

    return 0;
}
