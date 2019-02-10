---
title: Dockerコンテナ内のPostgresデータベースをダンプする方法
published: 2019-02-10
tags: Postgres, Docker
---

Dockerコンテナ内で起動しているPostgreSQLのデータベースをダンプしたいときがありまして, そのやり方を備忘録として残しておきます.

<!--more-->

## 基本構文

```
pg_dump --create --clean --if-exists --schema-only -U [user_name] [database_name] > [file_name]
```

### オプションについて

いくつかオプションを指定していますが, こちらは必要なものを選びましょう. 上の例で指定しているオプションについては解説をしておきます.

- `--schema-only`: 各テーブルに蓄えられているデータではなく, テーブル定義のみをダンプしている.

- `--crean`: `CREATE DATABASE`コマンドを出力.

- `--clean --if-exists`: `DROP DATABASE IF EXISTS`コマンドを出力.


## Docker
単体のコンテナを動かしている場合は以下のようになります.


```
$ docker exec [container] pg_dump --create --clean --if-exists --schema-only -U [user_name] [database_name] > [file_name]
```

括弧`[]`で囲んだ部分は人によって異なる部分です. 

## Compose

`docker-compose`を使って複数のコンテナを同時に動かしている場合は以下のようになります.

```
$ docker-compose exec [service] pg_dump --create --clean --if-exists --schema-only -U [user_name] [database_name] > [file_name]
```

括弧`[]`で囲んだ部分は人によって異なる部分です. 

## ポートフォワーディング
ポートフォワーディングしていて, 且つローカルマシンに`pg_dump`コマンドがインストールされている場合は以下のようにもできるかもしれません.

```
$ pg_dump --create --clean --if-exists --schema-only -h localhost -p [port] -U [user_name] [database_name] > [file_name]
```

ただし, ローカルマシンとコンテナの`pd_dump`のバージョンが一致していなくてはなりません. 一致していない場合は以下のようなエラーが出力され, ダンプができません.

```
pg_dump: server version: 11.1; pg_dump version: 10.6
pg_dump: aborting because of server version mismatch
```

せっかくコンテナを使っているのですから, コンテナの中で済ませたほうが良いでしょうね.
