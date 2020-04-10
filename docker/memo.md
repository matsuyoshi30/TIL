# Docker

### Docker image

```
$ docker pull nginx # nginx の docker image を docker hub から pull

$ docker run -d --name nginx-container -p 8181:80 nginx
```

`-d`でバックグラウンド実行指定、`-p`でポートフォワード


### CentOS 内で Tomcat インストール

```
$ docker run -it -d -p 18080:8080 -v /root/tomcat-container/logs:/share/logs --name tomcat centos:7
```

`-it`でコンテナ内にログイン、`-v`でホスト側のディレクトリとコンテナ内のディレクトリを共有

ホストとコンテナとのファイルやり取り

```
$ # ホストからファイルをコンテナ内にコピー
$ docker cp <ホスト側のファイル> <コンテナ名>:<コンテナ内のコピー先ディレクトリ>

$ # コンテナ内からホストへファイルコピー
$ docker cp <コンテナ名>:<コンテナ内のコピー元ファイル> <ホスト側のコピー先ディレクトリ>
```

コンテナからイメージを作成

```
$ docker commit <コンテナ名> <作成するDockerイメージ名>
```
