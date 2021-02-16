---
title: HRRからDocker内のPostgreSQLを操作してみた
published: 2019-03-16
tags: Haskell, HRR
toc: on
mathjax: on
---

SQLジェネレーターのHRR(Haskell Relational Record)を使ってみました. Dockerコンテナの準備からデータ挿入まで解説します.


<!--more-->

{toc}

プロジェクトの作成方法から丁寧に解説していきます(後々自分のためにもなるしね).

## 環境

- macOS Mojave (10.14.3)
- Docker version 18.09.1, build 4c52b90
- stack Version 1.9.3
- ghc version 8.6.4
- lts-13.12

## サンプルコード
[こちらのリポジトリ](git@github.com:logicoffee/hrr_experiments.git)で管理しています. 自由にご利用ください.


## 利用するパッケージ
主要パッケージを依存関係とともに紹介します.

- [relational-query-HDBC](http://hackage.haskell.org/package/relational-query-HDBC)
  - [HDBC](http://hackage.haskell.org/package/HDBC-2.4.0.2)
  - [relational-query](http://hackage.haskell.org/package/relational-query-0.12.1.0)
    - [persistable-record](http://hackage.haskell.org/package/persistable-record)
- [HDBC-postgresql](http://hackage.haskell.org/package/HDBC-postgresql-2.3.2.6)
  - [HDBC](http://hackage.haskell.org/package/HDBC-2.4.0.2)


`HDBC`はHaskellプログラムからDBに接続するための基本的な機能を提供しています. 今回はPostgreSQLを使うので`HDBC-postgresql`で定義されているPostgreSQL用のドライバを利用します.

基本的に`HRR`と言ったら`relational-query-HDBC`, `relational-query`, `persistable-record`の3つのこと指すんだと思います. いろんな機能があります(全然把握できてないのでブログ更新しつつ全体像つかめたらいいな). 日本人の方が中心となって開発されています.

## プロジェクトの作成

`stack new`で新規プロジェクトを作成します.

```none
stack --resolver lts-12.13 new hrr_experiments --bare
```

`--bare`オプションをつけることで, 現在のディレクトリに各ファイルを用意してくれます. このオプションをつけない場合は, 現在のディレクトリに`hrr_experiments`というディレクトリが作成され, その中に各ファイルが展開されます.

僕はいつもGitHubで空っぽのリポジトリを作成して, それをローカルに`clone`してくる方法をとっているため, `--bare`オプションが必要です.


## PotgreSQLの準備

### スキーマファイル
テーブルを定義するSQLファイルを用意します. 今回は`./db/docker-entrypoint-initdb.d`というディレクトリに以下のSQLファイルを作成しました.

```sql
-- ./db/docker-entrypoint-initdb.d/schema.sql

CREATE TABLE country (
  id SERIAL PRIMARY KEY,
  country_name text NOT NULL
);

INSERT INTO
  country (country_name)
VALUES
  ('Japan'),
  ('China'),
  ('Australia'),
  ('Russia');
```

文字列はシングルクォートで囲わなければならないところに注意ですね！(Haskellerがやらかしそうなミス)

テーブル名が単数形なのは後で説明します.

### DBサーバー

今回はローカルのDBサーバーとして, PostgresのDockerコンテナを利用します. プロジェクトルートに次のComposeファイルを用意しましょう.

```yaml
version: '3'
services:
  db:
    image: postgres:11.1-alpine
    environment:
      - POSTGRES_PASSWORD=password
      - POSTGRES_USER=user
      - POSTGRES_DB=test
    volumes:
      - ./db/data:/var/lib/postgresql/data
      - ./db/docker-entrypoint-initdb.d:/docker-entrypoint-initdb.d
    ports:
      - "5432:5432"
```

環境変数を上のように設定することで, ユーザー名やパスワードを設定することができます. ポートフォワーディングも忘れずに.

マウントしている2つのボリュームについても補足しておきましょう.

#### data
ひとつめはDBデータを永続的にするために設定しています. DBデータが常にプロジェクト配下の`./db/data`ディレクトリに保存されます.

#### docker-entrypoint-initdb.d
コンテナ内の`/docker-entrypoint-initdb.d`ディレクトリは特別な役割を持っています. それはコンテナ起動時に, このディレクトリ内にある`*.sql`, `*.sql.gz`, `*.sh`という拡張子を持ったファイルを実行してくれます.

つまりコンテナが起動し終わった時点で, `schema.sql`で定義したテーブルが用意された状態が得られるということですね.

## HRRからDBに接続
Haskellではあらゆるものが型で表現されるわけですが, HRRにおいてDBへの接続を表す型クラスは`HDBC`パッケージの`Database.HDBC`モジュールで定義されている`IConnection`です.

`HDBC-postgresql`パッケージでは`IConnection`のインスタンスである`Connection`という型が定義されています. これがDB接続を表現している型というわけですね.

### 依存パッケージの指定
利用するパッケージを設定ファイルに追加しましょう.

```yaml
# package.yaml

dependencies:
- base >= 4.7 && < 5
- HDBC
- HDBC-postgresql
- relational-query-HDBC
- relational-quer
```

```yaml
# stack.yaml

extra-deps:
- HDBC-postgresql-2.3.2.6
```

`HDBC-postgresql`はスナップショットに含まれていないので個別に指定する必要があります.

### DB接続
準備が整ったので, DBに接続するためのコードを書いていきましょう. `src`ディレクトリに`DB.hs`というファイルを用意してみました.

```haskell
-- ./src/DB.hs
module DB where

import Database.HDBC.PostgreSQL        (Connection, connectPostgreSQL)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)

connectPG :: IO Connection
connectPG = connectPostgreSQL $
    "host=localhost"
    ++ " port=5432"
    ++ " user=user"
    ++ " dbname=test"
    ++ " password=password"
    ++ " sslmode=disable"
```

これで終わりです. ユーザー名等はComposeファイルで設定したものを指定してくださいね.

## テーブルに対応した型の定義
HRRはデータベースに存在するテーブルに対応した型をTemplate Haskellを使って生成してくれます. やってみましょう.

### コード

`./src/Entity`に`Country.hs`を作成します.

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Entity.Country where

import Database.HDBC.Query.TH          (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import DB                              (connectPG)
import GHC.Generics                    (Generic)

$(defineTableFromDB
    connectPG
    driverPostgreSQL
    "public"
    "country"
    [''Show, ''Generic])
```

言語拡張がいかついですが, これが`Country`型を定義するためのコードです.

テーブル名をCamelCaseに直したものが型になります. `Countries`ではなく`Country`という型を作りたかったがために, `countries`ではなく`country`というテーブル名にしたのです.

### 生成された型を確認

本当に`Country`という型が生成されるのかを確認してみましょう.

まずはDBを起動しましょう.

```none
$ docker-compose up -d
```

次にGHCiを起動します.

```none
$ stack ghci
```

問題がなければ起動ができるはずです. するとこんな感じのプロンプトが表示されるかと思います.

```none
*Main DB Entity.Country>
```

ここで, 以下のコマンドを実行します.

```none
*Main DB Entity.Country> :browse Entity.Country
```

すると以下のような出力が得られるでしょう(適宜改行や空行を追加しています).

```haskell
data Country
  = Country {Entity.Country.id :: !GHC.Int.Int32,
             countryName :: !String}

columnOffsetsCountry :: GHC.Arr.Array Int Int
tableOfCountry :: Database.Relational.Table.Table Country
country :: Database.Relational.Monad.BaseType.Relation () Country

insertCountry :: Database.Relational.Type.Insert Country
insertQueryCountry ::
  Database.Relational.Monad.BaseType.Relation p Country
  -> Database.Relational.Type.InsertQuery p

Entity.Country.id' ::
  Database.Relational.Pi.Unsafe.Pi Country GHC.Int.Int32
countryName' :: Database.Relational.Pi.Unsafe.Pi Country String

selectCountry ::
  Database.Relational.Type.Query GHC.Int.Int32 Country
updateCountry ::
  Database.Relational.Type.KeyUpdate GHC.Int.Int32 Country
```

たしかに`Country`という型が定義されているのがわかりますね.

それに加え, insertやselect用の関数まで用意されています.

## クエリ発行

### コード

さて次はレコードの検索をしてみましょう. さっき作った`Country.hs`にコードを追加します.

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Entity.Country where

import Database.HDBC.Query.TH          (defineTableFromDB)
import Database.HDBC.Record.Query      (runQuery')        -- 追加
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Relational.Type        (relationalQuery)  -- 追加
import DB                              (connectPG)
import GHC.Generics                    (Generic)

$(defineTableFromDB
    connectPG
    driverPostgreSQL
    "public"
    "country"
    [''Show, ''Generic])

-- 以下を追加
showAllCountries :: IO ()
showAllCountries = do
    conn <- connectPG
    countries <- runQuery' conn (relationalQuery country) ()
    mapM_ print countries
```

これで`country`テーブルのレコードすべてを列挙する関数ができました.

### 関数の説明

ここで登場した関数を紹介します.

#### runQuery

`runQuery'`は`relational-query-HDBC`パッケージ内で定義されています.

```haskell
runQuery' :: 
    (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)	 
    => conn	     -- DB接続 ここではconnectPGの中身
    -> Query p a -- a型の値を返すクエリ ここではaはCountry
    -> p         -- パラメーターらしい ここでは使わないので()
    -> IO [a]    -- 検索結果
```

同じモジュール内で`runQuery`という関数も定義されていますが, こちらは`runQuery'`のlazyバージョンです.

#### relationalQuery

`relationalQuery`は`relational-query`パッケージで定義されています.

```haskell
relationalQuery :: Relation p r -> Query p r
```


上で見たように, HRRが生成した関数`country`は

```haskell
country :: Relation () Country
```

という型を持つので, 型が合っていることがわかりますね.


### 実行してみる

ではGHCi上で実行してみましょう.

```haskell
> Entity.Country.showAllCountries
Country {id = 1, countryName = "Japan"}
Country {id = 2, countryName = "China"}
Country {id = 3, countryName = "Australia"}
Country {id = 4, countryName = "Russia"}
```

## データ挿入

次はデータの挿入をやってみます.

`:browse`したときに, HRRが`insertCountry`という関数を作ってくれていることに気づいた方もいるかもしれません. しかし今回これは使いません.

この理由も含め, 順を追って説明していきます.

### コード

まずは追加するコードをお見せします.

```haskell
module Entity.Country where

-- これをimport
import Database.HDBC               (commit)
import Database.HDBC.Record.Insert (runInsert)
import Database.Relational.Type    (insert, relationalQuery)
-- その他importは省略

-- 以下を追加
testInsert :: IO ()
testInsert = do
    conn <- connectPG
    runInsert conn (insert countryName') "USA"
    commit conn
```

### コードの解説

`runInsert`は`relatinal-query-HDBC`パッケージで定義されています.

```haskell
runInsert ::
    (IConnection conn, ToSql SqlValue a)
    => conn       -- DB接続
    -> Insert a   -- SQLのINSERT文に対応
    -> a          -- 挿入データ
    -> IO Integer -- 挿入されたレコード数
```

HRRが生成した`insertCountry`を使うとしたら, 以下を考えることになります.

```haskell
runInsert ::
    (IConnection conn, ToSql SqlValue a)
    => conn
    -> Insert Country
    -> Country
    -> IO Integer
```

しかしここで問題が発生します. `Country`型の各フィールドは正格評価です. つまり`AUTO INCREMENT`される`id`も挿入時点で用意しなければならないということです.

これは面倒ですよね. というかやりたくありません.

というわけで, 考えたいのは以下です.

```haskell
runInsert ::
    (IConnection conn, ToSql SqlValue a)
    => conn
    -> Insert 国名
    -> 国名
    -> IO Integer
```

これを実現してくれるのが, `insert countryName'`なんです. 型を確認しておきましょう.

`insert`は`relational-query`パッケージで定義されています.

```haskell
insert ::
    (PersistableWidth r, TableDerivable r)
    => Pi r r'
    -> Insert r'
```

```haskell
countryName' :: Pi Country String
```

`Pi`という型は射影を表しています.

:::{.framed}
数学における射影とは, 例えば平面上の点から第一成分だけを取り出す操作

$$ (x,y) \mapsto x $$

などのことを言います.

`Country`は`id`と`countryName`の2つの成分を持っていて, そのうちの`countryName`だけを取り出す操作を表しているのが`countryName'`というわけです.
:::

### 実行してみる

GHCiで実行してみましょう. 挿入されたレコード数は捨てているので何も表示されませんが, たしかにレコードが登録されていることがわかります.

```haskell
> testInsert
> showAllCountries
Country {id = 1, countryName = "Japan"}
Country {id = 2, countryName = "China"}
Country {id = 3, countryName = "Australia"}
Country {id = 4, countryName = "Russia"}
Country {id = 5, countryName = "USA"}
```
