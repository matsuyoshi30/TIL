# POSIX 原理主義

POSIX = Portable Operating System Interface（UNIX系OS同士が互換性を持つための規格）

特徴

- OS間の互換性
  - いつでも
- 長寿命性
  - どこでも
- 即席性
  - すぐ

POSIX に準拠 = シェルスクリプトかC言語

パフォーマンスの面で、シェルスクリプトはC言語に劣る  
-> 手続き型からストリーミング型の書き方に改めることで処理効率は改善できる


```
# 手続き型（ステップ数が多い）
i=3
while [ $i -le 10000 ]; do
  file="file${i}.txt"
  rm -f $file
  i^$((i+3))
done

# ストリーミング型
aws `BEGIN(for(i=3;i<=10000;i+=3){print i;}}` |
sed 's/.*/file&.txt/'                         |
xargs rm -f
```

### 著者のプログラムによるシェルスクリプトの長寿命性の例

[メトロパイパー](metropiper.com)

東京メトロが Web API の公開に併せて開催したコンテストにおいて、締切から応募作品の62%が利用不可の状況になっていたが、上のメトロパイパーはメトロの API を叩いて得られる JSON を AWK や sed で解析して表示している POSIX 準拠のプログラムのため、2020年8月現在でも問題なく利用可能


悲劇が生まれる原因は、他人が作ったものに過剰に依存していることにある。20年間動き続ける保証のあるソフトウェアを手に入れるには、他人に頼らず自分で保守できる知恵を身につけること。

## 参考情報

- [シェルスクリプト入門](https://shellscript.sunone.me/)
- [Effective AWK Programming](http://www.kt.rim.or.jp/~kbk/gawk-30/gawk_toc.html)
- [UNIX/Linux の部屋](http://x68000.q-e-d.net/~68user/unix/)
- [会津大学UNIXウィキ](http://technique.sonots.com/)
- [Shellcheck (shellscript の linter)](https://www.shellcheck.net/)


## POSIX

https://pubs.opengroup.org/onlinepubs/9699919799/