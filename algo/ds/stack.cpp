#include <bits/stdc++.h>
#define rep(i, n) for(i = 0; i < n; i++)
using namespace std;

vector<int> input;

void push(int n) {
    input.push_back(n);
}

int pop() {
    int n = input[input.size()-1];
    input.pop_back();
    return n;
}

int main() {
    string line;
    getline(cin, line);

    vector<string> s;
    while (true) { // 空白で split
        int pos = line.find(" ");
        if (pos == string::npos) { // 最後
            s.push_back(line);
            break;
        }
        s.push_back(line.substr(0, pos));
        line = line.substr(pos+1, line.size()-pos-1);
    }

    for (int i=0; i<s.size(); i++) {
        if (s[i] == "+" || s[i] == "-" || s[i] == "*") {
            int val1 = pop();
            int val2 = pop();
            int val;
            if (s[i] == "+") val = val2 + val1;
            else if (s[i] == "-") val = val2 - val1;
            else if (s[i] == "*") val = val2 * val1;
            push(val);
        } else {
            int n = atoi(s[i].c_str());
            push(n);
        }
    }

    cout << input[0] << endl;

    return 0;
}
