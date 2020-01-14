#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

struct Node { // left-child, right-sibling
    int parent;
    int child;   // left
    int sibling; // right
};

#define NIL -1

vector<Node> tree;
vector<int> depth;

void setDepth(int u, int p) {
    depth[u] = p;
    if (tree[u].sibling != NIL)
        setDepth(tree[u].sibling, p); // right sibling -> same depth
    if (tree[u].child != NIL)
        setDepth(tree[u].child, p+1); // left child -> depth + 1
}

int main() {
    int n;
    cin >> n;
    int i;
    rep(i, n) { // init
        Node node;
        node.parent = NIL;
        node.child = NIL;
        node.sibling = NIL;
        tree.push_back(node);
    }
    rep(i, n) {
        depth.push_back(0);
    }

    int child;
    rep(i, n) { // input
        int id, sibcnt;
        cin >> id >> sibcnt;
        int j;
        rep(j, sibcnt) {
            int c;
            cin >> c;
            if (j == 0)
                tree[id].child = c; // left child
            else
                tree[child].sibling = c; // right sibling
            child = c;
            tree[c].parent = id;
        }
    }

    int root;
    rep(i, n) { // search root
        if (tree[i].parent == NIL)
            root = i;
    }

    setDepth(root, 0);

    rep(i, n) { // print
        cout << "node " << i << ": ";
        cout << "parent = " << tree[i].parent << ", ";
        cout << "depth = " << depth[i] << ", ";

        if (tree[i].parent == NIL)
            cout << "root, ";
        else if (tree[i].child == NIL)
            cout << "leaf, ";
        else
            cout << "internal node, ";

        cout << "[";
        int cl = tree[i].child;
        while (cl != NIL) {
            cout << cl;
            if (tree[cl].sibling == NIL)
                break;
            else
                cout << ", "; cl = tree[cl].sibling;
        }
        cout << "]" << endl;
    }

    return 0;
}
