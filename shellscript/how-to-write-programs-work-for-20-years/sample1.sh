#!/bin/bash
set -e

# どの環境でも使えるシェルスクリプト（文法・変数）

# expr コマンドを使えば計算できるが、 $((式)) も POSIX で規定されているので使用しても問題ない
expr 10 + 10
echo $((10+10))

# 数字の前に特定の文字を付与することで進数を変える
echo $((10+010))
echo $((10+0x10))

# --------------------------------------------------

# コマンド置換は `~` ではなく $(~) を使用する
$(echo $(echo echo x=)1)
# `echo `echo echo x=`1` と書くとエラー

# `~` 内のバックスラッシュはエスケープとして処理されるので、バグを招きやすい
printf 'printf a\\nb' | $(sed 's/\\/<backslash>/g')
printf 'printf a\\nb' | `sed 's/\\\\/<backslash>/g'`
# => a<backslash>nb
printf '\n'

# --------------------------------------------------

# else は処理するが then は何もしたくないときは、nullコマンド「:」を使う
if [ -s ./sample1.sh ]; then
    : # これがないとエラー
else
    rm /tmp/hoge.txt
fi

# --------------------------------------------------

# シェル関数内でローカルな変数を作る
# 小カッコえ囲まれた変数 $a, $b, $c は関数終了後に消滅する
localvar_sample() (
  a=$(whoami)
  b='My name is'
  c=$(awk -v id=$a -F : '$1==id{print $5}' /etc/passwd)
  echo "$b $c."
)

# --------------------------------------------------

# 環境変数など一般的な内容はスクリプトの冒頭で初期化する
# set -u
# umask 0022
# PATH='/usr/bin:/bin'
# IFS=$(printf ' \t\n_'); IFS=${IFS%_}
# export IFS LC_ALL=C LANG=C PATH

# --------------------------------------------------

# シェル変数
# $(変数 と何か) というパラメーター展開記述子を用いることで、変数の内容に基づいて部分的に取り出したり代替したりできる
# 記法はたくさんあるので省略

# --------------------------------------------------

# 乱数
# /dev/urandom を使うのが一般的
od -A n -t u4 -N 4 /dev/urandom | sed 's/[^0-9]//g'
# => 0 - 4294967295 の範囲で乱数生成

# /dev/urandom を使いたくない場合は ps コマンドの結果を利用する
LF=$(printf '\\n_');LF=${LF%_}  # sed のための改行定義
(ps -Ao pid,etime,pcpu,vsz; date) | # 乱数源（プロセス情報一覧＋日時）
od -t d4 -A n -v                  | # 数値化
sed 's/[^0-9]\{1,\}/'"$LF"'/g'    |
grep '[0-9]'                      |
tail -n 42                        |
sed 's/.*\(.\{8\}\)$/\1/g'        | # ここまでで awk に渡す乱数の種を生成
awk 'BEGIN{a=-2147483648;}        #
    {a+=$1;}                      #
    END{srand(a);print rand();}'

# --------------------------------------------------

# ロケールの影響を受けないよう、ロケール設定なしの状態（英語）で記述する

# env コマンドで全環境変数を無効化してコマンドを実行
echo 'ほげHOGE' | env -i awk '{print length($0)}'

# LC_ALL=C 及び LANG=C を設定して C ロケールにしてコマンドを実行
echo 'ほげHOGE' | LC_ALL=C LANG=C awk '{print length($0)}'

# あらかじめ LC_ALL=C LANG=C しておく
# export LC_ALL=C LANG=C



