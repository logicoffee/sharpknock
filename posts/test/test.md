---
title: テスト記事
published: 2019-01-20
tags: test, hakyll
mathjax: on
withtoc: on
---

ここまでがTeaser文.
ここまでがTeaser文.
ここまでがTeaser文.
ここまでがTeaser文.

<!--more-->


ここから本文.

## いち
### リスト

#### 簡単なリスト

- い
- ろ
- は

#### ネストしたリスト

- あ
  - あ
  - い
  - う
  - え
  - お
- か
  - か
    - か
    - が
  - き
    - き
    - ぎ 


### 引用
> sitation sitation sitation

### リンク
わからないことがあるときは[Googleさん](google.co.jp)に聞いてみよう.


## に
テーブル.

|   |column1|column2|
|:--|:----:|:------:|
|row1|は     |す|
|row2|け     |る|

## さん
シンタックスハイライト. 
```haskell
-- コメント
fibo :: [Int]
fibo = 1 : 1 : zipWith (+) fibo (tail fibo)
```

インライン`abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`.

## し
数式 $y=f(x)$ の表示.

$$
\sum_{n=0}^\infty \frac{1}{n!}
$$
