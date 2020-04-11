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


### Dockerfile

```
FROM centos:7
RUN yum install -y java
ADD files/apache-tomcat-9.0.6.tar.gz /opt/
CMD [ "/opt/apache-tomcat-9.0.6/bin/catalina.sh", "run" ]
```

`FROM`はベースとなる Docker image、`RUN`は OS のコマンドを実行、`CMD`は Dockerfile から作成した image 内でコマンド実行

`RUN`と`CMD`の違いは、`docker build`時に実行されるか、作成された image 内で実行されるか


```
$ docker build -t <Dockerイメージ名> <Dockerfileが存在するディレクトリ>
```

#### 注意点

Dockerfile の各コマンドが中間的なコンテナとして実行されるので、以下を意識する

- 前コマンドの内容が後ろで引き継がれない
  - `RUN cd /tmp`の後に`RUN touch test.txt`を実行しても、`test.txt`は`/`配下に作成されている
- キャッシュを考慮する
  - `docker build`時、 Dockerfile で既に実行されたコマンドについてはキャッシュが適用される
  - あまり変更が考えられないものは前の方に書いておく
- なるべくまとめて実行する
  - yum のインストールみたいなものは一つのコマンドで完了できるようにするか、キャッシュを意識した流れにする


### コンテナ間通信

Docker ネットワークを作成してコンテナ間通信を行う

```
$ docker network create <ネットワーク名>

$ docker network ls # ネットワーク一覧表示

$ docker network inspect <ネットワーク名> # ネットワーク内容確認
```

コンテナ起動時に`--network`でネットワークを指定して起動する

WordPress と MySQL の例

```
$ docker run --name mysql --network wordpress-network -e MYSQL_ROOT_PASSWORD=my-secret-pw -d mysql:5.7
$ docker run --name wordpress --network wordpress-network -e WORDPRESS_DB_PASSWORD=my-secret-pw -p 8080:80 -d wordpress
```

WordPress の Docker image では、 MySQL への接続先の指定として、`mysql`というホスト名を指定しているので、 MySQL のコンテナ起動時に名前を合わせる


### docker-compose

Docker ビルドやコンテナ起動のオプションなどを含め、複数のコンテナの定義をymlファイルに書くこどで、それを利用してDockerビルドやコンテナ起動ができるもの

一つの簡単なコマンドで複数のコンテナを管理できる

```yml
version: '3'
services:
  wordpress:
    image: wordpress
    container_name: some-wordpress
    restart: always
    ports:
      - 8080:80
    environment:
      WORDPRESS_DB_PASSWORD: my-secret-pw
  mysql:
    image: mysql:5.7
    container_name: some-mysql
    restart: always
    environment:
      MYSQL_ROOT_PASSWORD: my-secret-pw「
```

```
$ docker-compose build # 自分で Docker image を作成した場合

$ docker-compose up -d # コンテナをバックグラウンド起動
```
