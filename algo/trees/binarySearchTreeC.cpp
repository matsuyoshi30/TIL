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

Node* getMin(Node *x) {
    while (x->lc != NIL) {
        x = x->lc;
    }
    return x;
}

Node* getSuccessor(Node *x) {
    if (x->rc != NIL) return getMin(x->rc);

    Node *y;
    y= x->parent;
    while (y != NIL || x == y->rc) {
        x = y;
        y = y->parent;
    }
    return y;
}

void deleteNode(int k) {
    Node *r = root;
    while (r->key != k && r != NIL) {
        if (r->key < k) {
            r = r->rc;
        } else {
            r = r->lc;
        }
    }
    if (r == NIL) return;

    Node *x, *y;
    // 消す節点を決める
    if (r->lc == NIL || r->rc == NIL) y = r;
    else y = getSuccessor(r);
    // 消す節点の子を確認する
    if (y->lc != NIL) x = y->lc;
    else x = y->rc;

    // 消す節点の子の編集
    if (x != NIL) // 消す節点に子がいれば、その親には消す節点の親を設定
        x->parent = y->parent;

    // 消す節点の親の編集
    if (y->parent == NIL) { // 消す節点が全体の根
        root = x;
    } else if (y == y->parent->lc) { // 消す節点が、その親からみたときの左
        y->parent->lc = x;
    } else { // y == y->parent->rc // 消す節点が、その親から見たときの右
        y->parent->rc = x;
    }

    if (y != r) r->key = y->key;

    free(y);
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
        } else if (op == "delete") {
            cin >> input;
            deleteNode(input);
        }
    }

    return 0;
}
