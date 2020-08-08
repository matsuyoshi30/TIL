#!/bin/bash
set -eu

# どの環境でも使えるシェルスクリプト（コマンド）

# AWK

awk 'BEGIN{print -1*0}' # FreeBSD 9.x では -0 になる（他では0）
awk 'BEGIN{print -1*0+0}'

awk 'BEGIN{print 010;}' # FreeBSD では10, GNU AWK は 8
# 10進数扱いにしたい場合は文字列で渡す
awk 'BEGIN{print "010"*1;}'
echo 010 | awk '{print $1*1;}'

# 基本文法は各行に対するパターンとそれにマッチしたときのアクションの記述からなる
# echo HOGE | awk '1 END{exit;}' # アクション省略 -> エラーになる実装がある
echo HOGE | awk '{print;} END{exit;}'


# echo

# 以下のパターンについては echo コマンドを使わないほうが良い（OSによるオプションの差がある）
# - 先頭がハイフンで始まる可能性がある文字列
# - エスケープシーケンスを含む可能性のある文字列


# sed

printf 'Hello,\nWorld!' | sed '' # BSD版だと最後に改行コードが自動挿入されるが、GNU版ではされない


# which

# which は実は POSIX 標準ではない…
# POSIX に存在する command というコマンドで似たような動きが実現できる
which which >/dev/null 2>&1 || {
    which() {
	command -v "$1" 2>/dev/null |
	awk 'match($0,/^\//){print; ok=1;}
	     END {if(ok==0){print "which: not found" > "/dev/stderr"; exit 1;}}'
    }
}


# xargs

printf 'one two three' | xargs echo
