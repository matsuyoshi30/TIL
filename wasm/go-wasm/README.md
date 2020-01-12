# go-wasm

Go で wasm やってみる

[参考文献](https://buildersbox.corp-sansan.com/entry/2019/02/14/113000)

まずは`exec_wasm.js`を作業ディレクトリにコピー

```
$ cp /usr/local/Cellar/go/1.13.4/libexec/misc/wasm/wasm_exec.js .
```

環境変数を設定して、go で書いたコードを wasm にビルド

```
$ GOOS=js GOARCH=wasm go build -o main.wasm
```

参考文献では`js.NewCallback()`で Go で書いたメソッドを javascript のコールバック関数として呼び出せるようにしている

が、今は`js.FuncOf()`で設定する（戻り値も使えるようになってる）[stackoverflow](https://stackoverflow.com/questions/55800163/golangs-syscall-js-js-newcallback-is-undefined)


簡易的にサーバたてて確認

```
$ go get -u github.com/shurcooL/goexec
$ goexec 'http.ListenAndServe(":8080", http.FileServer(http.Dir(".")))'
```


