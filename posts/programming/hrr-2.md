---
title: HRRからPostgreSQL操作してみた【Part2】
published: 2019-03-20
tags: Haskell, HRR
toc: on
---

今回はHRRが自動で設定するデータ型を変更する方法をまとめます. 例として, PostgreSQLの`integer`がデフォルトで`Int32`に設定されてしまうのを`Int`で上書きしてみます.

<!--more-->

{toc}

## 前回の復習

[前回](/posts/programming/hrr.html)は,

- PostgreSQLのDockerコンテナを用意
- テーブルに対応したデータ型をHRRに生成してもらう
- HRRからDBを操作してみる

ということをやりました. 今回はちょっと細かめの話題です.

## やりたいこと

前回HRRが生成した`Country`型をもう一度見てみましょう.


```haskell
data Country
  = Country {Entity.Country.id :: !GHC.Int.Int32,
             countryName :: !String}
```

主キーである`id`カラムのデータ型が`Int32`になっています. これを`Int`にするのが, 今回やりたいことです.

## 方法

Template Haskellによって`Country`型を生成しているコードを修正します.

```haskell
-- 前回(抜粋)

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

```haskell
-- 今回(抜粋)

import Database.HDBC.Query.TH          (defineTableFromDB') -- 変更
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import DB                              (connectPG)
import GHC.Generics                    (Generic)

$(defineTableFromDB'
    connectPG
    driverPostgreSQL
    "public"
    "country"
    [("id", [t|Int|])] -- 追加
    [''Show, ''Generic])
```


これだけです. カラムのデータ型が指定できる`defineTableFromDB'`という関数を使えばいいだけでした.


## 試したけどうまくいかなかった方法

Haskell入門にはドライバーで設定する方法が書かれていました. それは次のような感じです.

```haskell
$(defineTableFromDB
    connectPG
    driverPostgreSQL { typeMap = [("integer", [t|Int|])] }
    "public"
    "country"
    [''Show, ''Generic])
```

これなら, 「PostgreSQLの`integer`型を一括でHaskellの`Int`型に設定できるじゃーん」と思ったのですが, ダメでした.

どうしてうまくいかないのか, 理由はわかりません...

