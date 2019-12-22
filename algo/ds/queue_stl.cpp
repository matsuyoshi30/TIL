#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

int main() {
    queue<string> q;

    q.push("apple");
    q.push("banana");
    q.push("cucumber");
    cout << q.size() << endl;

    cout << q.front() << " ";
    q.pop();
    cout << q.front() << " ";
    q.pop();
    cout << q.front() << endl;
    q.pop();

    if (q.empty()) {
        cout << "q is empty" << endl;
    }

    return 0;
}
