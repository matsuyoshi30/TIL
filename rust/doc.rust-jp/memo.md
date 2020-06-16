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

配列は、`[T; N]`という型を持ち、初期化のための省略表現がある

```rust
let a = [0; 20]; // a: [i32; 20]
```

a は、各要素が0で初期化された要素数20の配列となる


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


### 所有権

#### 所有権規則

- Rust の各値は、所有者と呼ばれる変数と対応している
- いかなるときも所有者は一人
- 所有者がスコープから外れたら、値は破棄される

以下のコードがコンパイルエラーになる

```rust
let s1 = String::from("hello");
let s2 = s1;

println!("{}, world!", s1);
```

`s2`に`s1`を代入することにより、`s1`に束縛された"hello"という値を保持するメモリ上のデータを`s2`が束縛することになる。
コンパイラはこの時点で`s1`が無効になると判断し、続く処理で`s1`を参照しているところでコンパイラエラーになる。

なぜか。
`s1`も`s2`も同じメモリ上のデータ領域を指しており、それぞれスコープを抜けるタイミングでdrop(freeに相当)しようとすると、二重開放エラーとしてメモリ安全性上のバグの一つを発生させる。
メモリ安全性を保証するために、`s1`の参照を無効なもの(`s1`は`s2`にムーブされた)と判定し、コンパイラエラーを出力する。

クローン`clone()`を使用することでデータのdeep copyが実現できる

```rust
let s1 = String::from("hello");
let s2 = s1.clone();

println!("s1 = {}, s2 = {}", s1, s2);
```

スタックのみのデータはコピー（以下）

- 整数型(`u32`など)
- 浮動小数点型(`f32`など)
- 論理値型`bool`
- 文字型`char`
- タプル(Copyの型のみを含む場合)


#### 参照と借用

所有権を取得して戻す一連の流れをすべての関数で実現するのはめんどくさい

```rust
fn main() {
    let s1 = String::from("hello");

    let (s2, len) = calculate_length(s1);

    //'{}'の長さは、{}です
    println!("The length of '{}' is {}.", s2, len);
}

fn calculate_length(s: String) -> (String, usize) {
    let length = s.len(); // len()メソッドは、Stringの長さを返します

    (s, length)
}
```

上記のように、`calculate_length()`のあとでも`s1`と同じデータの`s2`を返すよう戻り値をタプルにすることで実現できるが、やりたいことに比べて大げさ

その場合は「参照」を使う

```rust
fn main() {
    let s1 = String::from("hello");

    let len = calculate_length(&s1);

    // '{}'の長さは、{}です
    println!("The length of '{}' is {}.", s1, len);
}

fn calculate_length(s: &String) -> usize {
    s.len()
} // ここで、sはスコープ外になる。けど、参照しているものの所有権を持っているわけではないので
  // 何も起こらない
```

引数が`&String`となっている。このアンパサンドが参照を示し、所有権をもらうことなく値を参照することができる

関数の引数に参照を取ることを借用という。参照は不変であり、借用した値は変えることができない（コンパイルエラー）

```rust
fn main() {
    let mut s = String::from("hello");

    change(&mut s);
}

fn change(some_string: &mut String) {
    some_string.push_str(", world");
}
```

上記のように、引数に`&mut String`という可変な参照を取れば、借用した値を変更することができる

制約：特定のスコープで、ある特定のデータに対しては、一つしか可変な参照を持てない

複数の不変参照は可能だが、不変参照されている値を可変参照することはできない（不変参照しているということは、参照している値が不変であることを期待している）

ある関数内で生成した値の参照を関数外にわたすようなダングリング参照を防ぐ


#### スライス型

```rust
fn first_word(s: &String) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}
```

上記のように文字列スライスを返す関数を定義すると、下記のような使用方法ではコンパイルエラーになる


```rust
fn main() {
    let mut s = String::from("hello world");

    let word = first_word(&s);

    s.clear(); // error!
}
```

借用規則より、何らかの不変な参照があるときには可変の参照を得ることができない！

文字列リテラルはスライスなので、`fn first_word(s: &String) -> &str {`は`fn first_word(s: &str) -> &str {`というように書ける

```rust
fn main() {
    let my_string = String::from("hello world");

    // first_wordは`String`のスライスに対して機能する
    let word = first_word(&my_string[..]);

    let my_string_literal = "hello world";

    // first_wordは文字列リテラルのスライスに対して機能する
    let word = first_word(&my_string_literal[..]);

    // 文字列リテラルは、すでに文字列スライス*な*ので、
    // スライス記法なしでも機能するのだ！
    let word = first_word(my_string_literal);
}
```


### 構造体

構造体定義

```rust
struct StructName {
    // [field] name: type,
    propName1: String,
    propName2: u64,
}
```

インスタンス生成

```rust
let instance = StructName {
    propName1: String::from("value"),
    propName2: 10,
}
```

フィールドアクセスは`instance.propName1`でドット記法。インスタンスが可変mutの場合、ドット記法で特定のフィールドに値を代入できる

構造体更新記法を用いて、他のインスタンスから別のインスタンスを生成できる


```rust
let user2 = User {
    email: String::from("another@example.com"),
    username: String::from("anotherusername567"),
    ..user1 // user1 という別のインスタンスのフィールドの値を使用
};
```

#### 構造体使用例

```rust
#[derive(Debug)] // デバッグ情報出力用のアノテーション
struct Rect {
    width: u32,
    height: u32,
}

fn main() {
    let rect = Rect { width: 30, height: 50 };
    println!("rect is {:?}", rect); // 構造体rectにアノテーションを付与しているので使用できる

    println!("The area of the rectangle is {}", area(&rect)); // 借用
}

fn area(rectangle: &Rectangle) -> u32 {
    rectangle.width * rectangle.height
}
```

#### メソッド

構造体使用例をメソッドを用いて書き直す

```rust
#[derive(Debug)]
struct Rect {
    width: u32,
    height: u32,
}

impl Rect {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

fn main() {
    let rect = Rect { width: 30, height: 50 };
    println!("rect is {:?}", rect);

    println!("The area of the rectangle is {}", rect.area());
}
```

関連関数 = `self`を引数に取らない関数 -> コンストラクタとしてよく使用される（特定の値を引数にとり、それをフィールドの値としてインスタンスを生成する）


### Enum とパターンマッチ

Enum = 列挙型

Rust には`Option`型があり、パターンマッチ`match`制御フローでよく使用される

```rust
match x {
    None => None,
    Some(i) => Some(i + 1),
}
```

`match`は包括的であり、取りうるパターンがすべて列挙されていないとコンパイルエラー。考慮しなくてよい他のパターンは`_`というプレースホルダーで表せる

```rust
match x {
    Some(3) => println!("three"),
    _ => (),
}
```

上記のように、取りうるパターンのうち1つしか考慮しない場合は、わざわざ`match`でなくシンタックスシュガーである`if let`構文が使用できる

```rust
if let Some(3) = x {
    println!("three");
}
```
