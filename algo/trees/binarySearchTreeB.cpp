#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

struct Node {
    int key;
    Node *parent, *lc, *rc;
};

Node *root, *NIL;

void pre(Node *n) { // 先行順巡回
    if (n == NIL) return;
    cout << " " << n->key;
    pre(n->lc);
    pre(n->rc);
}

void in(Node *n) { // 中間順巡回
    if (n == NIL) return;
    in(n->lc);
    cout << " " << n->key;
    in(n->rc);
}

void insert(int k) {
    Node *y = NIL;
    Node *x = root;

    Node *z = (Node *)malloc(sizeof(Node));
    z->key = k;
    z->lc = NIL;
    z->rc = NIL;

    while (x != NIL) {
        y = x;
        if (z->key < x->key) {
            x = x->lc;
        } else {
            x = x->rc;
        }
    }
    z->parent = y;

    if (y == NIL) {
        root = z;
    } else if (z->key < y->key) {
        y->lc = z;
    } else {
        y->rc = z;
    }
}

bool find(Node *r, int k) {
    if (r == NIL) return false;
    if (r->key == k) {
        return true;
    } else {
        if (r->key < k) {
            return find(r->rc, k);
        } else { // r->key > k
            return find(r->lc, k);
        }
    }
}

void delete(int k) {
    // k があるかどうか find

    // k に子がいなければそのまま消す

    // k に子が一つあれば、それを消す対象の節点に設定する

    // k に子が二つあれば、k->rc を消す対象の節点に設定する（k->lc->key < k->rc->key なので）
}

int main() {
    int n, i;
    cin >> n;

    string op;
    int input;
    rep(i, n) {
        cin >> op;
        if (op == "insert") {
            cin >> input;
            insert(input);
        } else if (op == "print") {
            in(root);
            cout << endl;
            pre(root);
            cout << endl;
        } else if (op == "find") {
            cin >> input;
            if (find(root, input)) cout << "yes" << endl;
            else cout << "no" << endl;
        }
    }

    return 0;
}
