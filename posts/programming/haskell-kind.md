---
title: Haskellの型クラス宣言は中身まで見ないとKindがわからない
published: 2019-02-22
tags: Haskell
---

Haskellの型クラス宣言では, どんな関数が定義されているかまで見ないとKindが分かりません. 具体例で確かめてみました.

<!--more-->

以下のようなコードを書いてGHCi上に読み込んでみました.

```haskell
-- test.hs

class Bar x where
    bar :: x -> x

class Foo x where
    foo :: x a -> a
```

```haskell
> :l test.hs

> :k Bar
Bar :: * -> Constraint

> :k Foo
Foo :: (* -> *) -> Constraint
```
こんなふうに, `class`キーワードの部分の見た目は同じでも, それぞれが備えている関数によってKindが変わります.

`class`キーワードだけを見て, Kindが`* -> Constraint`だと思い込んで悩んでしまいました...
