---
title: (Scottyその1) HaskellのORMであるHRRを使う
published: 2019-02-24
tags: Haskell, Scotty
toc: on
---

HaskellのWebフレームワークであるScottyでWebアプリを作ることに挑戦しています. 今回はORMについてです.

<!--more-->

{toc}

## はじめに

ORMとはObject Relation Mappingのこと. データベースのレコードとプログラムのオブジェクトを関連付けてくれるやつですね. RailsでいうところのActiveRecordです.

今回Webアプリを作るにあたって, 通称HRRと呼ばれるライブラリを使うことにしました. [リポジトリ](https://github.com/khibino/haskell-relational-record)を見ていただければ分かるように, Contributerの多くが日本人です. Twitterでフォローさせていただいている方もいます.

ライブラリの専用サイトにチュートリアルがありますし, Haskell入門の第10章でも使われています. これらの情報を参考にモデルを設計してみました.

(resolverは`lts-13.8`を使用しています.)

## 前準備
### docker-compose
開発用のデータベースサーバーはDockerで動かしたいですよね. というわけでComposeファイルを書きましょう.

```yaml
version: '3'
services:
  db:
    image: postgres:11.1-alpine
    environment:
      - POSTGRES_PASSWORD=password
      - POSTGRES_USER=spicy
      - POSTGRES_DB=spicy
    ports:
      - "5432:5432"
```

[前記事](/posts/programming/scotty-webapp-1.html)で述べたように, 辛い食べ物を評価できるサービスを作ります. データベース名等が`spicy`なのはそのせいです.

### パッケージ
モデルを実装していくにあたり, 必要なパッケージを`package.yaml`に追加します. 一部を抜粋しましょう.

```yaml
executables:
  spicy:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -Wall
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - spicy
    - HDBC-postgresql
    - persistable-record
    - relational-query-HDBC
    - relational-query
```

さてこれだけでは不十分です. なぜなら`HDBC-postgres`は`lts-13.8`に含まれていないからです.

というわけで`stack.yaml`の`extra-deps`フィールドのコメントを外して, このパッケージを追記しましょう.

```yaml
extra-deps:
  - HDBC-postgresql-2.3.2.6
```
現時点の最新版を指定しました.

## 実装
以上で, モデル周りのコードを書く準備ができました. これからまずやっていくことはこれらのことです.

- データベースにテーブルを作成する
- データベースに存在するテーブルの定義からHaskellの型を定義する

2つめは自分でやるわけではありません. HRRにやってもらいます.

### コード
いくつか問題(後述)が発生しましたが, ひとまず動くコードを載せます.

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Model.AppUser where

-- defineTableFromDBで使うモジュール
import Database.HDBC.Query.TH          (defineTableFromDB)
import Database.HDBC.Schema.Driver     (typeMap)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import GHC.Generics                    (Generic)
import Model.DB                        (connectPG)

-- connectPGで使うモジュール
import Database.HDBC.PostgreSQL        (Connection, connectPostgreSQL)
import System.Environment              (getEnv)
import Text.Printf                     (printf)

defineTableFromDB
    connectPG
    driverPostgreSQL { typeMap = [("integer", [t|Int|])] }
    "public"
    "app_user"
    [''Show, ''Generic]

connectPG :: IO Connection
connectPG = do
    host     <- getEnv "PG_HOST"
    port     <- getEnv "PG_PORT"
    user     <- getEnv "PG_USER"
    db_name  <- getEnv "PG_DB_NAME"
    password <- getEnv "PG_PASSWORD"
    connectPostgreSQL $
        printf "host=%s port=%s user= %s dbname=%s password=%s sslmode=disable"
            host port user db_name password
```
### 解説

`defineTableFromDB`という関数により, データベースに存在するテーブルの定義からHaskellの型を作り出しています. この場合は`app_user`テーブルを見て`AppUser`型を作ってくれます.

ちなみにこの関数の型はこちら.

```haskell
defineTableFromDB
    :: IConnection conn
    => IO conn      -- データベース接続
    -> Driver conn  -- データベースドライバ
    -> String       -- スキーマ名
    -> String       -- テーブル名
    -> [Name]       -- ここで定義する型がderiveして欲しい型クラス
    -> Q [Dec]
```

さて上のコードについていくつか解説をします.

`connectPG`については, PostgreSQLを触ったことがあれば大体分かるかと. 環境変数からデータベース接続に必要な情報を集めています.

`driverPostgreSQL`では初期値を上書きしています. 「PostgreSQLの`integer`は, Haskell内では`Int`として扱ってね」という意味です. デフォルトでは`Int32`が使われます. Template Haskellの特殊な記法が使われていますね.

とくに指定しない場合は, テーブルが所属するスキーマは`public`になります.

最後に指定しているのが, 型`AppUser`が所属してほしい型クラスです. `Generic`は指定しないと怒られました. `Generic`のインスタンスを作るには`DeriveGeneric`という言語拡張が必要です.


### 問題点

#### テーブル名に悩んだ

テーブル名を複数形にすると, Haskellの型も複数形になってしまいます. それでは意味の正しくない型が出来上がってしまうため, テーブル名は単数形(というか作りたい型名)にする必要があります.

しかし問題なのは, `user`がPostgreSQLでは予約語であることです. したがって`user`に代わる単数形の単語を探さなければなりません... さすがに`user`が封じられるのは痛いですね. 類義語などを探してみましたがいいのは見つかりませんでした. 結局上のコードにあるように`app_user`を選択しました.

#### 参考にした情報よりも言語拡張が多い
このコードを書くために参考にしたのが, [公式のチュートリアル](http://khibino.github.io/haskell-relational-record/tutorial.html)とHaskell入門です. これら2つに書かれているコード例で指定されている言語拡張よりも, 上のコードに書いた言語拡張のほうが多いです. つけなかったら怒られるけどつけたらうまくいくのでこれで良いとは思うんですが...

#### `Int`にならない

`driverPostgreSQL`でデフォルトを上書きしていますが, これが反映されません. 理由はまだ分かっていません...


アプリケーション開発ができないほどの問題ではないのでこれで先に進もうと思います.
