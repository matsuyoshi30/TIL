#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

vector<string> bubbleSort(vector<string>& a, int n) {
    vector<string> b(a.size());
    copy(a.begin(), a.end(), b.begin());
    for (int i=0; i<n; i++) {
        for (int j=n-1; j>0; j--) {
            if (b[j][1] < b[j-1][1]) {
                swap(b[j], b[j-1]);
            }
        }
    }
    for (int i=0; i<n-1; i++) cout << b[i] << " ";
    cout << b[n-1] << endl;

    return b;
}

vector<string> selectionSort(vector<string>& a, int n) {
    vector<string> b(a.size());
    copy(a.begin(), a.end(), b.begin());
    for (int i=0; i<n; i++) {
        int min_j = i;
        for (int j=i; j<n; j++) {
            if (b[j][1] < b[min_j][1]) {
                min_j = j;
            }
        }
        swap(b[i], b[min_j]);
    }
    for (int i=0; i<n-1; i++) cout << b[i] << " ";
    cout << b[n-1] << endl;

    return b;
}

/*
 * ソート結果の安定性を確認
 * 組み合わせの総数が少ないので O(n^4) でも間に合う
 * 組み合わせが多い場合は、必ず安定するソートとの比較で確認する
 */
bool isStable(vector<string>& a, vector<string>& b, int n) {
    for (int i=0; i<n; i++) {
        for (int j=i+1; j<n; j++) {
            for (int x=0; x<n; x++) {
                for (int y=x+1; y<n; y++) {
                    if (a[i][1] == a[j][1] && a[i] == b[y] && a[j] == b[x]) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

int main() {
    int N;
    cin >> N;
    vector<string> A(N);
    for (int i=0; i<N; i++) cin >> A[i];

    vector<string> B = bubbleSort(A, N);
    if (isStable(A, B, N)) {
        cout << "Stable" << endl;
    } else {
        cout << "Not stable" << endl;
    }
    vector<string> S = selectionSort(A, N);
    if (isStable(A, S, N)) {
        cout << "Stable" << endl;
    } else {
        cout << "Not stable" << endl;
    }

    return 0;
}
