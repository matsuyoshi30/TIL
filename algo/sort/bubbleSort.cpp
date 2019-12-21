#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

/*
 * bubble sort
 *  未ソートの列の後ろから、隣接する要素の順序が逆だった場合は要素の入れ替えを実施する
 *  先頭はソート済みとなる
 *  入れ替えが行われなくなったら終了
 */
int bubbleSort(vector<int>& a, int n) {
    int cnt = 0;
    int flag = 1;
    while (flag) {
        flag = 0;
        for (int j=n-1; j>0; j--) {
            if (a[j] < a[j-1]) {
                int t = a[j];
                a[j] = a[j-1];
                a[j-1] = t;
                flag = 1;
                cnt++;
            }
        }

        // for (int i=0; i<a.size()-1; i++) cout << a[i] << " ";
        // cout << a[n-1] << endl;
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
    // cout << "----- start -----" << endl;

    int cnt = bubbleSort(A, N);
    cout << cnt << endl;

    return 0;
}
