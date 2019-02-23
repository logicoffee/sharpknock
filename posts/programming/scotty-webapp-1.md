---
title: 【Haskell】ScottyでWebアプリを作ってみる
published: 2019-02-23
tags: Haskell, Scotty
---

HaskellのWebフレームワークであるScottyを使ってWebアプリを作っていきたいなと考えています. あとから全部まとめるのは大変なので, 進捗が生まれる度に記事を書いていこうかなと.

<!--more-->

## フレームワークの選択
HaskellのWebフレームワークのフレームワークとしては以下のようなものがあります.

- Yesod
- Spock
- Scotty

この3つはHaskell入門で紹介されていたものです. 上のほうがより厚いフレームワークですね. この中からScottyを選ぶことにしました.

この3つ以外にもフレームワークはありますが, 書籍で取り上げられている安心感というものがあったので. それに使ってみないと良し悪しは分かりませんからね.

Spockにしようかなとも思ったのですが, 以下の理由からScottyにしました.

- 薄いほうが勉強になる
- Scottyのほうが開発が活発そう
- Scottyは最新のStackageスナップショット(lts-13.8)に含まれている


## Webアプリの制作方法をどう学ぶか

ここが一番難しいところですね. 以下の情報を駆使して頑張っていこうかなと思っています.

- Haskell入門の最後の2章
- RailsによるWebアプリ制作の経験
- Scottyリポジトリの`examples`ディレクトリ以下にあるサンプル集
- その他ネットの情報

Rails経験によって, 一通りのWebアプリ制作に必要なものはある程度把握しています. あとはそれをScottyやその他のライブラリから探してきて組み合わせるという作業になるかと思います.

このWebアプリ制作の主目的が勉強なので, これでOKです.

今後もこのブログで制作過程をまとめていきます.