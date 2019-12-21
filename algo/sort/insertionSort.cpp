#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

/*
 * insertion sort
 *  先頭の要素をソート済とする（スタートは先頭の次の要素から）
 *  未ソートの部分の先頭から、ソート済の列でどこに挿入されるかを操作する

 *  ある程度ソートされている列については高速に計算される
 *  基本的には O(n^2)
 */
void insertionSort(vector<int>& a, int n) {
    for (int i=1; i<n; i++) {
        int v = a[i];
        int j = i - 1;
        while (j >= 0 && a[j] > v) {
            a[j+1] = a[j];
            j--;
        }
        a[j+1] = v;

        for (int i=0; i<a.size()-1; i++) cout << a[i] << " ";
        cout << a[n-1] << endl;
    }
}

int main() {
    int N;
    cin >> N;
    vector<int> A(N);
    for (int i=0; i<N; i++) cin >> A[i];
    cout << "----- start -----" << endl;
    for (int i=0; i<N-1; i++) cout << A[i] << " ";
    cout << A[N-1] << endl;

    insertionSort(A, N);

    return 0;
}
