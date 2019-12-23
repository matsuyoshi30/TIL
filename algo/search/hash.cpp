#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

int i;

int charToInt(char c) {
    if (c == 'A') return 1;
    else if (c == 'C') return 2;
    else if (c == 'T') return 3;
    else if (c == 'G') return 4;
    else return 0;
}

long long stringToInt(string s) {
    int ret = 0;
    int p = 1, i = 0;
    rep(i, s.length()) {
        ret += p*charToInt(s[i]);
        p *= 5;
    }

    return ret;
}

#define HASH_VALUE 1046527
string dic[HASH_VALUE];

int hash1(int key) {
    return key % HASH_VALUE;
}

int hash2(int key) {
    return 1 + (key % (HASH_VALUE-1));
}

void insert(string s) {
    long long key = stringToInt(s);
    long long i, h;
    for (i=0; ; i++) {
        h = (hash1(key) + i * hash2(key)) % HASH_VALUE;

        if (dic[h] == s) {
            return; // 既に辞書に入れている
        } else if (dic[h].length() == 0) {
            dic[h] = s;
            return;
        }
    }
}

bool find(string s) {
    long long key = stringToInt(s);
    long long i, h;
    for (i=0; ; i++) {
        h = (hash1(key) + i * hash2(key)) % HASH_VALUE;

        if (dic[h] == s) return true;
        else if (dic[h].length() == 0) return false;
    }
    return false;
}

int main() {
    int n;
    cin >> n;
    rep(i, HASH_VALUE) dic[i] = "";
    char how[6], str[14];
    rep(i, n) {
        scanf("%s %s", how, str);
        if (how[0] == 'i') {
            insert(str);
        } else { // find
            if (find(str))
                cout << "yes" << endl;
            else
                cout << "no" << endl;
        }
    }

    return 0;
}
