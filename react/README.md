# React

公式のドキュメントを見ながら今更やってみる

> コンポーネントは JavaScript の関数と似ています。（“props” と呼ばれる）任意の入力を受け取り、画面上に表示すべきものを記述する React 要素を返します。

### イベンド処理

> 一般的に、onClick={this.handleClick} のように () を末尾に付けずに何らかのメソッドを参照する場合、そのメソッドはバインドしておく必要があります。

```js
// This binding is necessary to make `this` work in the callback
this.handleClick = this.handleClick.bind(this);
```