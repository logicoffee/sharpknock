---
title: Haskellの正格評価と遅延評価について実験してみた
published: 2019-04-27
tags: Haskell
---

遅延評価はどこまで遅延されるのかよく分かってなかったので, いくつか実験をしてみました.


<!--more-->

次のコードは問題なく動き, 実行結果は`3`です. タプルの第二要素は評価されなかったということですね.

```hs
main = print $ fst $ (3, undefined)
```

同様に以下も動きます.

```hs
data Human = Human
    { name :: String
    , age  :: Int
    } deriving(Show)

main :: IO ()
main = do
    let h = Human undefined 3
    print $ age h
```

パターンマッチもOKです.

```hs
data Human = Human
    { name :: String
    , age  :: Int
    } deriving(Show)

main :: IO ()
main = do
    let Human x y = Human undefined 3
    print y
```

ネストしていてもOKです. `FullName`は各フィールドが正格評価ですが, `Human`自体は`name`が`undefined`でも問題ありません.

```hs
data Human = Human
    { name :: FullName
    , age  :: Int
    } deriving(Show)

data FullName = FullName
    { first  :: !String
    , second :: !String
    } deriving(Show)

main :: IO ()
main = do
    let h = Human undefined 3
    print $ age h
```
