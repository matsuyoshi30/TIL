#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

/// TLE
// int fib(int n) {
//     if (n == 0 || n == 1) {
//         return 1;
//     }
//     return fib(n-1) + fib(n-2);
// }

int main() {
    int n;
    cin >> n;
    vector<int> f(n+1);
    f[0] = 1;
    f[1] = 1;;
    for (int i=2; i<=n; i++) {
        f[i] = f[i-1] + f[i-2];
    }

    cout << f[n] << endl;

    return 0;
}
