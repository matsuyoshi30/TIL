# 作業メモ

## style を scss で書こうと思っても node-sass のバージョンで怒られる

`yarn add node-sass sass-loader`して Vue の style を scss で書こうと思ったが、`yarn serve`で以下のエラー

```
Module build failed (from ./node_modules/sass-loader/dist/cjs.js):
Error: Node Sass version 5.0.0 is incompatible with ^4.0.0.
    at getSassImplementation (/Users/matsuyoshi/projects/til/vuejs/sample-project/node_modules/sass-loader/dist/utils.js:84:13)
    at Object.loader (/Users/matsuyoshi/projects/til/vuejs/sample-project/node_modules/sass-loader/dist/index.js:34:59)
```

とりあえず `yarn add node-sass@4`でバージョン固定して追加したら解決

## SPA的な

`vue add router`でルーター追加 & プロジェクトを更新

`/router/index.js`でURLに対応するコンポーネントの動的なimport

`/views`以下にはルートに対応するコンポーネントごとにファイルをつくる

## データストアに関する問題

マウント = コンポーネントのオブジェクトをインスタンス化してDOMに置き換える処理

マウントのたびに data() が呼ばれ*初期状態*を生成

router で SPA にしても前のページに戻ったりするとデータが初期状態になり、それが微妙なケースもある（ログイン情報とか）

Vuejs のコンポーネント間のデータやりとりは props down か event up で基本的に親子関係のコンポーネントで行われる

===> 公式から提供されている Vuex という状態管理ライブラリをつかおう
