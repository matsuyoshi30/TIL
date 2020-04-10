#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

int main() {
    int n;
    cin >> n;
    int p[n+1];
    for (int i=1; i<=n; i++)
        cin >> p[i-1] >> p[i];

    // calc
    int m[n+1][n+1];
    for (int i=1; i<=n; i++) m[i][i] = 0;
    for (int len=2; len<=n; len++) {
        for (int i=1; i<=n-len+1; i++) {
            int j = i + len - 1;
            m[i][j] = INT_MAX;
            for (int k=i; k<j; k++)
                m[i][j] = min(m[i][j], m[i][k] + m[k+1][j] + p[i-1] * p[k] * p[j]);
        }
    }

    cout << m[1][n] << endl;

    return 0;
}
