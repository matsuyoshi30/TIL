#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

/*
 * ソート対象の列の長さ g に対して挿入ソートを実施
 */
int insertionSort(vector<int>& a, int n, int g) {
    int cnt = 0;
    for (int i=g; i<n; i++) {
        int v = a[i];
        int j = i - g;
        while (j >= 0 && a[j] > v) {
            a[j+g] = a[j];
            j = j - g;
            cnt++;
        }
        a[j+g] = v;
    }
    return cnt;
}

/*
 * shell sort
 *  挿入ソートがそれなりにソートされている列に対して高速に働くのを利用したソート
 *  データを完全にソートするために、最後の間隔（ g ）は1で挿入ソートを実施する
 */
void shellSort(vector<int>& a, int n) {
    // G を準備
    vector<int> G; // 3g+1: 1, 4, 13, 40, ...
    for (int val=1; val<=n; val=val*3+1) {
        G.push_back(val);
    }
    cout << G.size() << endl;

    for (int i=G.size()-1; i>0; i--) cout << G[i] << " ";
    cout << G[0] << endl;

    int cnt = 0;
    for (int i=G.size()-1; i>=0; i--) {
        cnt += insertionSort(a, n, G[i]);
    }
    cout << cnt << endl;
}

int main() {
    int N;
    cin >> N;
    vector<int> A(N);
    for (int i=0; i<N; i++) cin >> A[i];

    shellSort(A, N);
    for (int i=0; i<N; i++) cout << A[i] << endl;

    return 0;
}
