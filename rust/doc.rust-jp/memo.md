# Rust

### installation

[公式サイト](https://www.rust-lang.org/tools/install)の通りにやってみたら権限周りでエラーになった

```
error: could not amend shell profile: '/Users/matsuyoshi/.profile'
error: caused by: could not write rcfile file: '/Users/matsuyoshi/.profile'
error: caused by: Permission denied (os error 13)

$ ls -l ${HOME}/.*profile
lrwxr-xr-x  1 matsuyoshi  staff  33 Aug 25  2018 /Users/matsuyoshi/.bash_profile
-rw-r--r--  1 root        staff  31 Aug 16  2017 /Users/matsuyoshi/.profile
```

`modify PATH variable`を no にしてインストールできた

ツールチェインとして、Rust のインストーラーである`rustup`、 Rust のコンパイルを行う`rustc`、パッケージマネージャーの`cargo`がインストールされる。
でも`cargo`しかほぼ触らないらしい（たしかに`cargo`以外あまり見ない気がする）

#### tools

補完や定義ジャンプ用の`racer`をインストール、、、nightly じゃないといけないらしい

```
$ rustup install nightly && cargo +nightly install racer
```


### start project

```
$ cargo new <project name>

$ cd <project name>

$ cargo run
Hello, world!
```

ハローワールドまでがはやい


### 数当てゲーム

[日本語ドキュメント](https://doc.rust-jp.rs/book/second-edition/)の2章やってみての雑多なメモ

変数は`let`で基本不変。`let mut`で mutable 可変な変数として宣言。

あるメソッドから返ってくる`Result`型の値に対して`expect`メソッドを呼び出すことでエラー処理ができる。
で、`Result`型の値が返ってきてるのに`expect`メソッドを呼び出していない場合はコンパイラが警告を出す。便利！

依存するパッケージ（ Rust ではクレート）を`Cargo.tonl`の [dependencies] 以下にバージョンとともに記載する。
`cargo build`を実行すると、追記したクレートのバージョンをダウンロードして`Cargo.lock`が更新される。`Cargo.lock`は`go.mod`+`go.sum`みたいな

```rust
extern crate rand;

use rand::Rng;
```

1行目は`rand`クレートを外部依存として使用するための行。だけど削除しても動いた。

3行目は`gen_range()`メソッドを使用するための`Rng`トレイトをスコープ内とするもの。

**TODO** トレイトとは？

「どのクレートでどのトレイトを使えばええんや。。」問題は、外部クレートをダウンロードしてあれば、`cargo doc --open`を実行することででローカルでダウンロードしたクレートのドキュメントを閲覧できるのでそれ使う。

```rust
match <value> {
    // method が返す可能性のあるパターン１
    // method が返す可能性のあるパターン２
    // method が返す可能性のあるパターン３
}
```

`match`式の冒頭で与えた値（メソッドから返ってくる値など）が一致するパターンを各アーム（後ろのブロックで記載されるもの）と照合していき、マッチしたアームを実行する

`let <variablename>: <type>`で型を注釈して変数宣言


### 普遍的なプログラミング概念

#### 変数と可変性

変数は基本不変で`mut`つければ可変

不変な変数と定数`const`は違う

不変な変数でも`let`キーワードを使用して同じ変数名を再び使用することが出来る＝シャドーイング

```rust
let spaces = "  ";         // 空文字（文字列）
let spaces = spaces.len(); // 長さ（数値）
```

シンプルな変数名を再利用することが可能


#### データ型

基本的に型推論してくれるけど注釈が必要な場合は型を明記する（しないとコンパイルエラー）

大きく分けてスカラー型と複合型の二種類

- スカラー型
  - 整数: i8, i16, i32(基準型), i64, u8, u16, u32, u64
  - 浮動小数点: f32, f64(基準型)
  - 論理値: bool
  - 文字: char ユニコード(not ASCII)
- 複合型
  - タプル
  - 配列

タプルについては以下参照

```rust
let tup = (100, 6.4, 1); // 違う型でもよい
let (x, y, z) tup; // 分配
let a = tup.0 // 添字でアクセス可能
```


#### 関数

Rust は式志向言語

```rust
fn a() {
    println!("no return");
}

fn b(x: i32) -> i32 {
    x + 1 // 関数ブロック内の最後の式（;無し）を戻り値として返す
}
```

式と文の見分け方＝終端に`;`があるかないか（あったら文）


#### フロー制御

`if`は式なので以下のような書き方ができる

```rust
let number = if condition {
    5
} else {
    6
};
```

当然 if-else 節内でデータ型が違えばコンパイルエラー

ループは`loop`, `while`, `for`の3つ。最も使われるのは`for`

```rust
let a = [10, 20, 30];
for element in a.iter() {
    // do something
}
```

Range型を使うと以下のようにかける

```rust
for number in (1..4) {
    // do something from 1 to 3
}
```
