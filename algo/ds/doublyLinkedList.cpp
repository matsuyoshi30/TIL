#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

struct Node {
    int val;
    Node *prev, *next;
};

Node *nil;

void initialize() {
    nil = (Node *)malloc(sizeof(Node));
    nil->next = nil;
    nil->prev = nil;
}

void insert(int val) { // nil -> [new node] -> ..
    Node *n = (Node *)malloc(sizeof(Node));
    n->val = val;

    n->next = nil->next;
    nil->next->prev = n;
    n->prev = nil;
    nil->next = n;
}

Node* linearSearch(int val) {
    Node *cur = nil->next;
    while (cur != nil && cur->val != val) {
        cur = cur->next;
    }
    return cur;
}

void deleteNode(Node *t) {
    if (t == nil) return;
    t->next->prev = t->prev;
    t->prev->next = t->next;
    free(t);
}

void deleteFirst() {
    deleteNode(nil->next);
}

void deleteLast() {
    deleteNode(nil->prev); // はじめに insert した要素が末尾の要素 nil->prev
}

void deleteVal(int val) {
    deleteNode(linearSearch(val));
}

void printNode() {
    Node *cur = nil->next;
    while (cur != nil) {
        cout << cur->val;
        cur = cur->next;
        if (cur != nil) cout << " ";
    }
    cout << endl;
}

int main() {
    int n;
    cin >> n;
    initialize();
    string command;
    int num;
    int i;
    rep(i, n) {
        cin >> command;
        if (command == "insert") {
            cin >> num;
            insert(num);
        } else if (command == "deleteFirst") {
            deleteFirst();
        } else if (command == "deleteLast") {
            deleteLast();
        } else { // delete
            cin >> num;
            deleteVal(num);
        }
    }

    printNode();

    return 0;
}
