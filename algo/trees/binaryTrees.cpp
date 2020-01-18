#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

struct Node {
    int parent;
    int lc, rc;
};

#define NIL -1

vector<Node> tree;
vector<int> depth;
vector<int> height;

void setDepth(int u, int p) {
    depth[u] = p;
    if (tree[u].lc != NIL) setDepth(tree[u].lc, p+1);
    if (tree[u].rc != NIL) setDepth(tree[u].rc, p+1);
}

int setHeight(vector<int>& h, int u) {
    int hl = 0, hr = 0;
    if (tree[u].lc != NIL) hl = setHeight(h, tree[u].lc) + 1;
    if (tree[u].rc != NIL) hr = setHeight(h, tree[u].rc) + 1;

    return h[u] = max(hl, hr);
}

int main() {
    int n;
    cin >> n;
    int i;
    rep(i, n) { // init
        Node node;
        node.parent = NIL;
        node.lc = NIL;
        node.rc = NIL;
        tree.push_back(node);
    }
    rep(i, n) {
        depth.push_back(0);
    }
    rep(i, n) {
        height.push_back(0);
    }

    rep(i, n) { // input
        int id, l, r;
        cin >> id >> l >> r;
        tree[id].lc = l;
        tree[id].rc = r;
        if (l != NIL) tree[l].parent = id;
        if (r != NIL) tree[r].parent = id;
    }

    // search root
    int root;
    rep(i, n) {
        if (tree[i].parent == NIL)
            root = i;
    }

    setDepth(root, 0);
    setHeight(height, root);

    // print
    int sib, deg = 0;
    string type;
    rep(i, n) {
        sib = NIL;
        if (tree[i].parent != NIL) {
            if (tree[tree[i].parent].lc != i && tree[tree[i].parent].lc != NIL)
                sib = tree[tree[i].parent].lc;
            else if (tree[tree[i].parent].rc != i && tree[tree[i].parent].rc != NIL)
                sib = tree[tree[i].parent].rc;
        }

        deg = 0;
        if (tree[i].lc != NIL) deg++;
        if (tree[i].rc != NIL) deg++;

        cout << "node " << i << ": ";
        cout << "parent = " << tree[i].parent << ", ";
        cout << "sibling = " << sib << ", ";
        cout << "degree = " << deg << ", ";
        cout << "depth = " << depth[i] << ", ";
        cout << "height = " << height[i] << ", ";

        if (tree[i].parent == NIL) type = "root";
        else if (height[i] == 0) type = "leaf";
        else type = "internal node";

        cout << type << endl;
    }

    return 0;
}
