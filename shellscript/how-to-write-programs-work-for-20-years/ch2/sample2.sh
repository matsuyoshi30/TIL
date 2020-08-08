#!/bin/bash
set -e

# どの環境でも使えるシェルスクリプト（正規表現）

# https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html

# BRE(Basic Regular Expression) meta character

## ブラケット外部のメタ文字
### ^ -> 文字列の先頭
### $ -> 文字列の末尾
### [...] -> [ と ] で囲まれた中で列挙した文字のいずれか1文字
### [^...] -> [^ と ] で囲まれた中で列挙した文字以外の任意の1文字
### * -> 繰り返し指定子。直前に記述した文字が0文字以上連続
### \{n\} -> 繰り返し指定子。直前に記述した文字がn文字連続
### \{n,\} -> 繰り返し指定子。直前に記述した文字がn文字」以上連続
### \{m,n\} -> 繰り返し指定子。直前に記述した文字がm文字以上n文字以下連続
### \{...\} -> 包括指定子。範囲...の文字列を繰り返し指定子の1文字として扱う
### \n -> 後方参照子。n番目に記した包括指定子でマッチした文字列
 #### ABC123ABCABC
 #### ^\([A-Z]*\)123\1*$ -> \1 が ABC とみなされる

## ブラケット内部のメタ文字
### ^ -> 否定
### - -> 範囲

## 置換後の文字列指定（s/A/B/ のB）で使用できるメタ文字
### \n -> n番目に記した包括指定子でマッチした文字列
### & -> マッチした文字列全体


# ERE(Extended Regular Expression)

## BREとの違い
## - 使えるメタ文字が追加（+, ?, | など）
## - バックスラッシュでエスケープしていたカッコ類がバックスラッシュ不要（非互換）
## - 後方参照(\n)が無保証（非互換）


# AWK
echo "http://www.kt.rim.or.jp/~kbk/gawk-30/gawk_5.html"
