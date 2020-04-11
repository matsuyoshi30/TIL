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
