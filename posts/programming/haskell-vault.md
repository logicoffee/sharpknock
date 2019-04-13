---
title: Haskellのvaultの使い方
published: 2019-04-13
tags: Haskell
toc: on
---

Haskellのパッケージvaultの使い方をご紹介します. 「vault」とは金庫室という意味の英単語です. その名の通りこのパッケージが提供する機能は「値の保管」です.

<!--more-->

{toc}

## 使い方
用意されている関数の型を見ればだいたい使い方がわかります. ここでは`Data.Vault.Lazy`の関数定義一覧を載せておきます.

```haskell
-- からっぽの金庫を作成
empty :: Vault

-- 金庫の鍵を新規作成
newKey :: IO (Key a)

-- 金庫に保管されている値を取り出す
lookup :: Key a -> Vault -> Maybe a 

-- 金庫に値を保管する
insert :: Key a -> a -> Vault -> Vault 

-- 金庫に保管されている値を変換する
adjust :: (a -> a) -> Key a -> Vault -> Vault 

-- 鍵を消去する
delete :: Key a -> Vault -> Vault 

-- 2つの金庫を1つにまとめる
union :: Vault -> Vault -> Vault
```


では使ってみましょう. 以下のサンプルコードでは, 先頭に

```haskell
import qualified Data.Vault.Lazy as V
```

がついていると思って読んでください.

### 素直に使ってみる

```haskell
main = do
    key <- V.newKey

    let vault' = V.insert key "hello" V.empty

    print $ V.lookup key vault
```

```haskell
-- 実行結果
Just "hello"
```

### 上書きする
`insert`関数はそれまでの値を上書きします.

```haskell
main = do
    key <- V.newKey

    let vault = V.insert key "overwrite"
              $ V.insert key "hello"
                V.empty

    print $ V.lookup key vault
```

```haskell
-- 実行結果
Just "overwrite"
```

### 複数の鍵を使う
上書きされる性質により, ひとつの鍵に対してはひとつの値しか保管できないということです.

値を複数保管したい場合は, 鍵もそれと同じ数だけ用意する必要があります.

```haskell
main = do
    key1 <- V.newKey
    key2 <- V.newKey

    let vault = V.insert key2 "value2"
              $ V.insert key1 "value1"
                V.empty

    print $ V.lookup key1 vault
    print $ V.lookup key2 vault
```

```haskell
-- 実行結果
Just "value1"
Just "value2"
```

### 異なる型の値を保管する
型`Vault`の定義には型引数`a`が使われていません. したがって異なる型の値を保持できるということです.

```haskell
main = do
    key1 <- V.newKey
    key2 <- V.newKey

    let vault = V.insert key2 (1 :: Int)
              $ V.insert key1 "value1"
                V.empty

    print $ V.lookup key1 vault
    print $ V.lookup key2 vault
```

```haskell
-- 実行結果
Just "value1"
Just 1
```

### 鍵を消去する
```haskell
main = do
    key <- V.newKey

    let vault = V.delete key
              $ V.insert key "value"
                V.empty

    print $ V.lookup key vault
```

```haskell
-- 実行結果
Nothing
```


## Vaultはどこで使われているのか？
そもそもなぜ`vault`パッケージの存在を知ったのかというと, これを使っているライブラリを使う機会があったからです.

そのライブラリとは[wai](http://hackage.haskell.org/package/wai)パッケージです. Webアプリケーションの制作には欠かせないライブラリですね.

`wai`には`Request`というリクエスト情報を扱うための型が定義されています. この`Request`はセッションID等の情報を保管するために, `Vault`型の値を保持しています. Scottyのような薄いライブラリを使うとなると, セッション管理のためのミドルウェアを用意する必要があり, 必然的に`Vault`型の値を扱うことになるんじゃないかなと思います.
