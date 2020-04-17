
### 20200418

[task](https://atcoder.jp/contests/abc149/tasks/abc149_c)

解答範囲内の素数を全列挙して vector に詰めてそれと入力値を比較するという面倒なやり方していた

入力値からやればいい単純に。

### 20200415

[task](https://atcoder.jp/contests/abc142/tasks/abc142_c)

「生徒の出席番号、その生徒が何番目に出席したか」を pair として vector に詰めて、何番目に出席したかでソートすればOK

pair のソートは以下の通り

```cpp
// pair<int, int> a
bool cmp(pair<int, int> a, pair<int, int> b) {
    return a.second < b.second;
}

sort(a.begin(), a.end(), cmp);
```

### 20200402

[task](https://atcoder.jp/contests/cf16-final/tasks/codefestival_2016_final_b)

1 から順番に足していって N を越えたとき、欲しい配列（解く問題の配点の最大値）を得られる。
出力するときは合計が N になるよう調整して余計なものを省いて出力。

### 20200401

[task](https://atcoder.jp/contests/code-festival-2017-quala/tasks/code_festival_2017_quala_b)

総当り。問題の入力制約を見ればアタリが付きそう。

### 20200331

[task](https://atcoder.jp/contests/mujin-pc-2016/tasks/mujin_pc_2016_b)

考え方はあっていたが、最小の半径の考察で漏れがあった。

```cpp
double m = (a+b+c)*(a+b+c)*PI; // max
double n = a-b-c>0 ? (a-b-c)*(a-b-c)*PI : 0; // min
```

単純に、len(OA)-len(AB)-len(BC) が 0 より大きいかで判定していたのが誤り。
線分を入れ替えても結果が変わらないことにきづけるかどうかがポイント。

len(OA), len(AB), len(BC) を昇順に x, y, z とすると、 z > x+y or z <= x+y の条件となる。


