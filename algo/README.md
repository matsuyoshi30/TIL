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

#### 安定したソート

ソートの安定性（比較対象において同じ要素がソートした結果同じ順番になるか）を検証

バブルソートや挿入ソートは必ず安定するが、選択ソートは安定なソートではない

[ALDS1_2_C](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4064626)

##### 挿入ソート（insertion sort）

[ALDS1_1_A](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4061510)

##### バブルソート（bubble sort）

[ALDS1_2_A](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4064629)

##### 選択ソート（selection sort）

[ALDS1_2_B](http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=4064630)

