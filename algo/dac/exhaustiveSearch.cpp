#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

int i, n;
vector<int> A;

bool exhaustiveSearch(int i, int sum) {
    if (sum == 0) return true;
    if (i >= n) return false;
    bool ret = exhaustiveSearch(i+1, sum) || exhaustiveSearch(i+1, sum-A[i]);
    return ret;
}

int main() {
    cin >> n;
    int t = 0;
    rep(i, n) {
        cin >> t;
        A.push_back(t);
    }
    int q;
    cin >> q;
    rep(i, q) {
        int m;
        cin >> m;
        if (exhaustiveSearch(0, m)) cout << "yes" << endl;
        else cout << "no" << endl;
    }

    return 0;
}
