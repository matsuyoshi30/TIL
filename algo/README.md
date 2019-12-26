# Algorithm and Data structure

## 基本的な考え方

与えられた問題に対して、以下の点を考慮して解答を検討する

- 問題の質（問われている答えの種類）
- 入力の大きさ
- 制約
  - 時間的制約
  - 領域的制約
  
[手始め](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4058822)


## ソート

以下の点に留意する

- 計算量と安定性（同じ要素がソート後も同じ順番）
- 入力データの特性と計算量への影響
- データの列を保持する配列以外のメモリが必要かどうか

### 安定したソート

ソートの安定性（比較対象において同じ要素がソートした結果同じ順番になるか）を検証

バブルソートや挿入ソートは必ず安定するが、選択ソートは安定なソートではない

[ALDS1_2_C](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4064626)

#### 挿入ソート（insertion sort）

[ALDS1_1_A](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4061510)

#### バブルソート（bubble sort）

[ALDS1_2_A](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4064629)

#### 選択ソート（selection sort）

[ALDS1_2_B](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4064630)

### シェルソート（shell sort）

挿入ソートが、それなりに既にソートされている列に対しては皇族に働くという性質を利用したソートアルゴリズム

ソート対象の列で、指定した間隔で挿入ソートをずらしながら実施する

[わかりやすいアニメーション](https://www.youtube.com/watch?v=n4sk-SzGvZA)

[ALDS1_2_D](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4064888)


## データ構造

### 初等的なデータ構造

#### スタック（stack）

Last In First Out。一時的にデータを退避したいときに有効

- push(x): スタックのトップに要素を追加
- pop(): スタックのトップから要素を取り出す
- isEmpty(): スタックが空かどうか判定
- isFull(): スタックが満杯かどうか判定

[ALDS1_3_A](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4064998)

#### キュー（queue）

First In First Out。データを到着順に処理したいときに利用

- enqueue(x): キューの末尾に要素を追加
- dequeue(): キューの先頭から要素を取り出す
- isEmpty(): キューが空かどうか判定
- isFull(): キューが満杯かどうか判定

キューの処理は、dequeue() を実行していちいち先頭を消していると、そのたびに O(n) の計算が必要になる

[時間切れ](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4065280)

キューは単なる配列や vector ではなく、リングバッファとして実装することで、上記の問題を解決する

[リングバッファ](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4065294)

スタックやキューは C++ の標準ライブラリにある（std::stack, std::queue）ので、それ使える

[stack](./ds/stack_stl.cpp)
[queue](./ds/queue_stl.cpp)

#### 連結リスト

各要素が前後の要素へのポインタを保持しているリスト

```cpp
struct Node {
    int val;
    Node *prev, *next;
};
```

いわゆる「番兵」として空要素を用意しておき、要素の挿入や探索は番兵を利用する

[ALDS1_3_C](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4065578)


## 探索

- 線形探索
  - 番兵使うことで、ループの終了条件だけ気にすれば良くなる（番兵まで行く = ない、なのでキーとの比較が不要）
- 二分探索
  - 事前にソートされている必要がある
  - O(logN) で動作するので線形探索より効率が良い
  - C++ では lower_bound という STL が提供されている
- ハッシュ法
  - ハッシュテーブルと、そのテーブル上のどの位置に挿入するかを要素から算出するハッシュ関数を使用
  - めっちゃ TLE した。。原因は C++ の string における length() と size() の差？要確認
    - グローバル変数にしてた i の関数内での初期化忘れだった。。
    - [TLE](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_4_C/judge/4069161/C++)
  - [ALDS1_4_C](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_4_C/judge/4069162/C++)

## 再帰と分割統治法

問題を小さく分割（Devide）して、小さい問題を解いて（Solve）、解を結合（Conquer）する方法


