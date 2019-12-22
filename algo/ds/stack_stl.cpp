#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

int main() {
    stack<int> s;

    s.push(3);
    s.push(1);
    s.push(4);
    cout << s.size() << endl;

    cout << s.top() << " ";
    s.pop();
    cout << s.top() << " ";
    s.pop();
    cout << s.top() << endl;
    s.pop();

    if (s.empty()) {
        cout << "s is empty" << endl;
    }

    return 0;
}
