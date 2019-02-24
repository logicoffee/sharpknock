---
title: gormのマイグレーション機能の挙動を確かめてみた.
published: 2019-02-19
tags: Go
toc: on
---

GoのORMであるjinzhu/gormのマイグレーション機能の挙動を確かめてみました.

<!--more-->

{toc}

## スペック
- go version go1.11.5
- gorm v1.9.2
- postgreSQL 11.1

## ファイル構成
Webアプリを作って行く途中でいろいろ試したので無駄にファイルが分かれていますがお気になさらず...


`main.go`では`models.InitDB()`を呼び出してるだけです.
```go
// main.go

package main

import (
	"github.com/ほにゃらら/models"
)

func main() {
	models.InitDB()
}
```

`InitDB()`では, データベースに接続して`AutoMigrate()`しています. ほんとは`db.Close()`ちゃんとしたほうがいいんでしょうけどとりあえずこれで.
```go
// db.go

package models

import (
	"fmt"
	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/postgres"
	"os"
)

var db *gorm.DB

func InitDB() {
	connectDB()
	db.AutoMigrate(&Player{})
}

func connectDB() {
	var err error
	db, err = gorm.Open(
		"postgres",
		fmt.Sprintf(
			"host=%s user=%s dbname=%s password=%s sslmode=disable",
			os.Getenv("PG_HOST"),
			os.Getenv("PG_PORT"),
			os.Getenv("PG_USER"),
			os.Getenv("PG_DB"),
			os.Getenv("PG_PASSWORD"),
		),
	)
	if err != nil {
		panic(err)
	}
}
```

`Player`構造体の定義をいろいろ変えてみて, テーブル定義がどうなるのかを見ていきます.

## マイグレーションを実行してみよう
`models.go`内の`Player`構造体の定義をいくつか試します.

### Goのデータ型との対応
まずはGolangの型がpostgreSQLのカラム定義ではどうなるのかを試してみます.

```go
import (
	"time"
)

type Player struct {
	Name      string
	Age       int
	Birthday  time.Time
	Activated bool
	Struct    Address
	Pointer   *string
}
```

Column    |           Type           | Collation | Nullable | Default 
:---------|:-------------------------|:----------|:---------|:---------
name      | text                     |           |          | 
age       | integer                  |           |          | 
birthday  | timestamp with time zone |           |          | 
activated | boolean                  |           |          | 
pointer   | text                     |           |          | 

配列型, スライス型, 関数型については, `invalid sql type`と怒られました. 構造体は怒られはしませんが, データベースには反映されていません.

また, インデックスやカラム制約も定義されていませんでした.

### exportされていないフィールド
構造体に小文字から始まる(つまりexportされていない)フィールドがある場合を試してみます.

```go
type Player struct {
	Name         string
	passwordHash string
	deleted      bool
}
```
結果はこんな感じ.

Column | Type | Collation | Nullable | Default 
:------|:-----|:----------|:---------|:--------
name   | text |           |          | 

exportされていないフィールドはデータベースに反映されないんですね.




### gorm.Model をインポート
gormで用意されている基本的なフィールドを利用してみます.

```go
type Player struct {
	gorm.Model
	Name string
}
```

この構造体は次のものと等価です.

```go
type Player struct {
	ID        uint `gorm:"primary_key"`
	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt *time.Time
	Name      string
}
```

この構造体に対するテーブル定義は次のようになりました.

Column     |           Type           | Collation | Nullable |               Default               
:----------|:-------------------------|:----------|:---------|:------------------------------------
id         | integer                  |           | not null | nextval('players_id_seq'::regclass)
created_at | timestamp with time zone |           |          | 
updated_at | timestamp with time zone |           |          | 
deleted_at | timestamp with time zone |           |          | 
name       | text                     |           |          | 
Indexes:
    "players_pkey" PRIMARY KEY, btree (id)
    "idx_players_deleted_at" btree (deleted_at)

### 関連テーブル
次は関連テーブルを定義してみます. [公式ドキュメント](http://doc.gorm.io/associations.html#belongs-to)に書かれているとおりに構造体を定義しました. 

```go
type Player struct {
	Name      string
	Address   Address
	AddressID int
}

type Address struct {
	gorm.Model
	Prefecture string
	City       string
}
```

また, `Address`もマイグレート出来るように`InitDB`の中身を以下のように変更しておきます.

```go
func InitDB() {
	connectDB()
	db.AutoMigrate(&Player{}, &Address{})
}
```

するとそれぞれのテーブル定義は次のようになりました.

#### players

Column     |  Type   | Collation | Nullable | Default 
:----------|:--------|:----------|:---------|:--------
name       | text    |           |          | 
address_id | integer |           |          | 

#### addresses

Column     |           Type           | Collation | Nullable |                Default                
:----------|:-------------------------|:----------|:---------|:--------------------------------------
id         | integer                  |           | not null | nextval('addresses_id_seq'::regclass)
created_at | timestamp with time zone |           |          | 
updated_at | timestamp with time zone |           |          | 
deleted_at | timestamp with time zone |           |          | 
prefecture | text                     |           |          | 
city       | text                     |           |          | 
Indexes:
    "addresses_pkey" PRIMARY KEY, btree (id)
    "idx_addresses_deleted_at" btree (deleted_at)

外部キー制約は定義できないようです.

## まとめ

構造体の定義をそのまま使えるのは便利ですね. しかし外部キー制約を指定することができなかったり, ロールバック機能が無かったりするので, しっかりしたアプリを構築する場合は別のマイグレーションツールを使ったほうが良いかもしれません.
