#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

struct Process {
    string name;
    int time;
};

#define LEN 100000
Process pa[LEN];
int head, tail;

void enqueue(Process p) {
    pa[tail] = p;
    tail = (tail+1) % LEN;
}

Process dequeue() {
    Process pr = pa[head];
    head = (head+1) % LEN;
    return pr;
}

int main() {
    int n, q;
    cin >> n >> q;
    for (int i=0; i<n; i++) cin >> pa[i].name >> pa[i].time;

    head = 0;
    tail = n;
    int restime = 0;
    while (head != tail) {
        Process pc = dequeue();
        int min_t = min(q, pc.time);
        pc.time -= min_t;
        restime += min_t;
        if (pc.time > 0) {
            enqueue(pc);
        } else {
            cout << pc.name << " " << restime << endl;
        }

    }

    return 0;
}
