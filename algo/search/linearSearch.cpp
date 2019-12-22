#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

int i;
int n, q;
vector<int> s;

// int linearSearch(int key) {
//     for (int i=0; i<n; i++) {
//         if (s[i] == key)
//             return i;
//     }
//     return -1;
// }

int linearSearch(int key) {
    int i = 0;
    s[n] = key; // 番兵
    while (s[i] != key)
        i++;
    if (i == n) // 番兵まで行った = なかった
        return -1;
    return i;
}

int main() {
    int input;
    vector<int> ans;

    cin >> n;
    rep(i, n) {
        cin >> input;
        s.push_back(input);
    }
    s.push_back(0); // 番兵用

    cin >> q;
    rep(i, q) {
        cin >> input;
        int t = linearSearch(input);
        if (t >= 0) ans.push_back(t);
    }
    ans.erase(unique(ans.begin(), ans.end()), ans.end());
    cout << ans.size() << endl;

    return 0;
}
