#include <bits/stdc++.h>
using namespace std;

int calcLCS(string s, string t) {
    if (s.length() < t.length()) {
        string temp = s;
        s = t;
        t = temp;
    }

    int table[s.length()+1][t.length()+1];
    for (int i=0; i<=s.length(); i++) {
        table[i][0] = 0;
    }
    for (int j=0; j<=t.length(); j++) {
        table[0][j] = 0;
    }

    for (int i=1; i<=s.length(); i++) {
        for (int j=1; j<=t.length(); j++) {
            if (s.at(i-1) == t.at(j-1)) {
                table[i][j] = table[i-1][j-1] + 1;
            } else {
                table[i][j] = max(table[i][j-1], table[i-1][j]);
            }
        }
    }

    return table[s.length()][t.length()];
}

int main() {
    int n;
    cin >> n;
    for (int i=0; i<2*n; i+=2) {
        string x, y;
        cin >> x >> y;

        cout << calcLCS(x, y) << endl;
    }

    return 0;
}
