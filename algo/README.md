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

### 高等的

#### マージソート（merge sort）

高速かつ安定的、だが入力データを保持する領域以外に一時的にメモリが必要

[ALDS1_5_B](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_5_B/judge/4091051/C++)

#### パーティション（partition）

与えられた数列の最後の要素を基準値として、配列内の要素を二種類に分割する  
処理は O(n) だが離れた要素を処理するので、ソート時の利用には注意が必要  

[ALDS1_6_B](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_6_B/judge/4091302/C++)

#### クイックソート（quick sort）

上記のパーティションを用いたソート

パーティション->分割した2つの配列に対してクイックソート

 -> それぞれに対してパーティション->クイックソート...

というふうに分割して再帰的に処理する

追加のメモリを必要としないが、離れた要素を交換するので安定ではない

**ソート結果が安定かどうかは、安定的なソートの結果と比較すればよい！**

[ALDS1_6_C](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_6_C/judge/4093246/C++)

#### 計数ソート（counting sort）

累積和を用いて O(n+k) の線形時間で処理される高速なソート

[ALDS1_6_A](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_6_A/judge/4095217/C++)

### STL

#### sort()

```cpp
vector<int> v;
sort(v.begin(), v.end());

int l[5];
sort(l, l+5);
```

クイックソートがベースとなっており、 O(n logn) で動作する高速なソート

ただし安定ではない

安定なソートを STL を用いてやりたい場合は stable_sort() を利用する

マージソートがベースとなっており、安定で O(n logn) ではあるが、追加領域が必要かつ速度も sort() のほうが速い


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

再帰的な構造を持つ図形（フラクタル）の描画例 [Koch Curve](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_5_C/judge/4084523/C++)


## 木構造

節点（node）とそれを結ぶ辺（edge）で表現されるデータ構造

### 根付き木

根（root）という、ほかの node とは区別された node を持つ木

木とグラフの違いは root があるか否か

[ALDS1_7_A](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_7_A/judge/4107706/C++)

node の数がもう変化しないことがわかっているので、 left-child right-sibling representation で木構造を表現する

木構造の表現を高変化させることで、多分木を二分木に変換できる！

### 二分木

根付き木よりも最初からシンプルに書ける

深さや高さの計測は再帰的に処理する（高さは左右の子の高さの大きい方）

[ALDS1_7_B](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_7_B/judge/4116025/C++)

### 巡回

木構造のデータを探索するには *再帰* を用いる

一番シンプルな木構造（根、左ノード、右ノードそれぞれひとつずつ）に対してどのように巡回するかを定義した再帰的な関数を作成することで、全体の木構造の根 root を渡せば全体を再帰的に巡回できる

[ALDS1_7_C](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_7_C/judge/4135499/C++)

先行順巡回：根ノード、左、右の順で巡回  
中間順巡回：左、根ノード、右の順で巡回  
後行順巡回：左、右、根ノードの順で巡回  


### 二分探索木

二分探索条件を満たす木構造のデータ

節点をポインタで連結させることで木構造を表現する

```
struct Node {
    int key;
    Node *parent, *left, *right;
};
```

ポインタで連結させる場合は、`(Node *)malloc(sizeof(Node))`で領域を確保し、アロー演算子`n->key`で値にアクセスして作成する

入力に偏りがない場合は O(logN) の計算量

[挿入](https://onlinejudge.u-aizu.ac.jp/status/users/matsuyoshi/submissions/1/ALDS1_8_A/judge/4143653/C++)

