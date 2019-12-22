#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

int i;
int n, q;
vector<int> s;

int binarySearch(int key) {
    int start = 0, end = n;
    int mid;
    while (start < end) {
        mid = (end+start) / 2;
        if (s[mid] == key) return mid;
        if (s[mid] < key) {
            start = mid + 1;
        } else {
            end = mid;
         }
    }
    return -1;
}

int main() {
    int input;
    vector<int> ans;

    cin >> n;
    rep(i, n) {
        cin >> input;
        s.push_back(input);
    }

    cin >> q;
    rep(i, q) {
        cin >> input;
        int t = binarySearch(input);
        if (t >= 0) ans.push_back(t);
    }
    ans.erase(unique(ans.begin(), ans.end()), ans.end());
    cout << ans.size() << endl;

    return 0;
}
