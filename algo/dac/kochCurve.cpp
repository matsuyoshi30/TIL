#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

struct Point {
    double x;
    double y;
};

double r = M_PI * 60.0 / 180.0;

void koch(int d, Point p1, Point p2) {
    if (d == 0) return;

    // calculate s, t
    Point s, t;
    s.x = (p2.x - p1.x)/3 + p1.x;
    s.y = (p2.y - p1.y)/3 + p1.y;
    t.x = ((p2.x - p1.x)/3)*2 + p1.x;
    t.y = ((p2.y - p1.y)/3)*2 + p1.y;

    Point u;
    // calculate u
    u.x = (t.x - s.x) * cos(r) - (t.y - s.y) * sin(r) + s.x;
    u.y = (t.x - s.x) * sin(r) + (t.y - s.y) * cos(r) + s.y;

    koch(d-1, p1, s);
    cout << s.x << " " << s.y << endl;
    koch(d-1, s, u);
    cout << u.x << " " << u.y << endl;
    koch(d-1, u, t);
    cout << t.x << " " << t.y << endl;
    koch(d-1, t, p2);
}

int main() {
    int n;
    cin >> n;

    Point start, end;
    start.x = 0.00000;
    start.y = 0.00000;
    end.x = 100.00000;
    end.y = 0.00000;

    cout << start.x << " " << start.y << endl;
    koch(n, start, end);
    cout << end.x << " " << end.y << endl;

    return 0;
}
