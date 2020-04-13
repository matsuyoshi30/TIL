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

```
extern crate rand;

use rand::Rng;
```

1行目は`rand`クレートを外部依存として使用するための行。だけど削除しても動いた。

3行目は`gen_range()`メソッドを使用するための`Rng`トレイトをスコープ内とするもの。

**TODO** トレイトとは？

「どのクレートでどのトレイトを使えばええんや。。」問題は、外部クレートをダウンロードしてあれば、`cargo doc --open`を実行することででローカルでダウンロードしたクレートのドキュメントを閲覧できるのでそれ使う。

```
match <値> {
    // method が返す可能性のあるパターン１
    // method が返す可能性のあるパターン２
    // method が返す可能性のあるパターン３
}
```

`match`式の冒頭で与えた値（メソッドから返ってくる値など）が一致するパターンを各アーム（後ろのブロックで記載されるもの）と照合していき、マッチしたアームを実行する

`let <variablename>: <type>`で型を注釈して変数宣言


