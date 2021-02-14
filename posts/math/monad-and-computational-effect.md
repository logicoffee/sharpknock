---
title: モナドはなぜモナド則を満たさなければならないのか？
published: 2019-04-30
mathjax: on
toc: on
tags: Haskell, モナド, 圏論
---

モナドという概念の存在意義やモナド則の意味について, 自分なりの理解を記事にしました.

<!--more-->

## はじめに

- Haskell等でモナドという概念に触れていることを前提とします.

- またこの記事では圏論の基礎的な知識を仮定します(クライスリ圏とかモノイド対象とかいう用語は出てこないから安心して！). 

- 私自身は情報系出身ではないため, 計算機科学については詳しくありません. あくまで自分なりの理解を記したに過ぎないことをご了承ください.


{toc}

## 参考文献

- [圏論の歩き方](https://www.amazon.co.jp/%E5%9C%8F%E8%AB%96%E3%81%AE%E6%AD%A9%E3%81%8D%E6%96%B9-%E5%9C%8F%E8%AB%96%E3%81%AE%E6%AD%A9%E3%81%8D%E6%96%B9%E5%A7%94%E5%93%A1%E4%BC%9A/dp/4535787204)

この本ではでは圏論が各分野でどんなふうに使われているかがさらっと解説されています.

第5章は「モナドと計算効果」というタイトルで書かれています.

## モナド則とは

Haskellにおける型クラス`Monad`とは次のようなものです.

```haskell
class Monad m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b 
    return :: a -> m a
```

ただし2つの関数`(>>=)`と`return`は以下の等式を満たす必要があります. これらがいわゆるモナド則です.

```haskell
return a >>= f  == f a
m >>= return    == m
(m >>= f) >>= g == m >>= (\x -> (f x >>= g))
```

実はこのモナド則は次のように書き換えることもできます.

```haskell
join . fmap (fmap f) == fmap f . join
join . fmap join     == join . join
join . return        == join . fmap return == id
```

ただし

```haskell
fmap :: (a -> b) -> m a -> m b
join :: m (m a) -> m a
```

であり,

```haskell
fmap f m == m >>= (return . f)
join n   == n >>= id
m >>= g  == join (fmap g m)
```

という関係が成り立っています.

本記事では, モナドがモナド則を満たして然るべきであることを見ていきますが, モナド則としては上記のうち後者で考えます.

また, 2つのモナド則が等価であることは省略します(どなたかが記事にしていましたが, どれだったか忘れてしまいました...).

## モナドの役割

まず次のような純粋なプログラムを考えるための圏$\mathcal{C}$を考えます.

:::{.framed}
**圏$\mathcal{C}$の定義**

- $\mathrm{Ob}(\mathcal{C}) =$ 型全体

- $\mathrm{Hom}_{\mathcal{C}}(a,b) = a$型の値を入力すると$b$型の値を返す関数全体

ただし射の合成は関数の合成として定義する.

:::

例えば $\mathrm{Int}\in\mathrm{Ob}(\mathcal{C})$ で, $x\mapsto x^2 \in \mathrm{Hom}(\mathrm{Int}, \mathrm{Int})$ です.


この圏の各射は, 何を入力して何を出力するか, という情報しか持っていません. つまり副作用が表現できていないというわけです.

しかし現実世界の多くのプログラムは副作用を持ちます. 副作用をも表現できる圏を考えたいというのが人情ですね.

ではどうしたら良いでしょうか？

次のように, 入力と出力以外の情報を表現できるようにしてみましょう.

$$ f : a \to M(b) $$

出力型として$b$ではなく, $b$を何かしらの関数$M$に適用したものにしています. 現時点では$M$の正体はブラックボックスですが, どうにかして$b$以外の情報を持たせようというアイデアです.

以後この味付け関数$M$をモナドと呼びましょう.

## モナドに期待する性質

漠然とモナド$M$を導入してみただけなので, $M$は型を別の型に変換する関数であるということ以外に分かっていることはありません.

他にモナドに期待する性質はあるでしょうか？

$M$ は $\mathrm{Ob}(\mathcal{C})\to\mathrm{Ob}(\mathcal{C})$ という形の関数です. どうせなら自己関手

$$ M : \mathcal{C}\to\mathcal{C} $$

になってくれたら便利そうですよね. これはつまり, $M$ が単に型 $a$ を $M(a)$ に変換するだけでなく, 関数 $f:a\to b$ を $M(f): M(a) \to M(b)$ に変換する役割も持っていることになります.

以下モナド $M$ は自己関手であるとします.

さて私達は, 「副作用をもつ関数も扱える圏を考えたい」のでしたよね. では次に, そのような圏$\mathcal{C}'$を構成していきましょう.

:::{.framed}

**圏$\mathcal{C}'$の定義**

- $\mathrm{Ob}(\mathcal{C}') = \mathrm{Ob}(\mathcal{C})$
- $\mathrm{Hom}_{\mathcal{C}'}(a, b) = \mathrm{Hom}_{\mathcal{C}}(a, M(b))$

:::

副作用に関する情報を表現するために $a\to M(b)$ という形の関数を考えましたが, 新しい圏 $\mathcal{C}'$ においてはあくまで $a \to b$ という形の射として扱うのがポイントです.

ひとまず圏 $\mathcal{C}'$ の構成要員を列挙しましたが, これが本当に圏を成すかは保証されていません. 

これから $\mathcal{C}'$ が圏になるためにはどんなことが成り立てばいいかを考えていきます. 種明かしをすれば, これは実はモナド則なのです.

さて圏の定義を思い出しましょう. チェックしなければならないことが3つありますね.

- 射の合成ができる
- 射の合成は結合律をみたす
- 恒等射が存在する

ひとつずつ見ていきます.

### 射の合成

$f\in \mathrm{Hom}_{\mathcal{C}'}(a, b),\ g\in\mathrm{Hom}_{\mathcal{C}'}(b,c)$ は, 圏$\mathcal{C}$の中で見ると

$$
\begin{aligned}
f&: a \to M(b)\\
g&: b \to M(c) 
\end{aligned}
$$

という形の射です. これらを合成して,

$$ a \to M(c) $$

という形の射を作らなくてはなりません.

そのために(天下り的ですが)自然変換 $join: M^2 \to M$ を導入します. すると

$$
g\circ f : a \xrightarrow{f} M(b) \xrightarrow{M(g)} M^2(c) \xrightarrow{join_c} M(c)
$$

のようにほしい関数が得られました.

さて, $join$ が自然変換であることを等式で表現しておきましょう.

任意の射 $f: a\to M(b)$ に対し以下の図式が可換になることが自然変換の定義でしたから,

$\require{AMScd}$

$$
\begin{CD}
M^2(a) @>{M^2(f)}>> M^3(b)\\
@V{join_a}VV        @VV{join_{M(b)}}V\\
M(a)   @>>{M(f)}>   M^2(b)
\end{CD}
$$

次が成り立ちます.

:::{.framed}
**モナドの性質1**

$$ join_{M(b)}\circ M^2(f) = M(f)\circ join_a $$

:::

### 結合律

$f\in \mathrm{Hom}_{\mathcal{C}'}(a, b),\ g\in\mathrm{Hom}_{\mathcal{C}'}(b,c),\ h\in \mathrm{Hom}_{\mathcal{C}'}(c, d)$ に対し, $(h \circ g) \circ f$と$h \circ (g \circ f)$をそれぞれ書き下すと次のようになります.

$$
\begin{aligned}
(h \circ g) \circ f &=
    a \xrightarrow{f}
    M(b) \xrightarrow{M(g)}
    M^2(c) \xrightarrow{M^2(h)}
    M^3(d) \xrightarrow{M(join_d)}
    M^2(d) \xrightarrow{join_d}
    M(d)\\[1em]
h \circ (g \circ f) &=
    a \xrightarrow{f}
    M(b) \xrightarrow{M(g)}
    M^2(c) \xrightarrow{join_c}
    M(c) \xrightarrow{M(h)}
    M^2(d) \xrightarrow{join_d}
    M(d)\\
\end{aligned}
$$

$join$ は自然変換ですから,

$$ join_{M(d)}\circ M^2(h) = M(h)\circ join_c $$

が成り立つんでしたよね. これを $h \circ (g \circ f)$ に適用すると次のようになります.

$$
\begin{aligned}
(h \circ g) \circ f &=
    a \xrightarrow{f}
    M(b) \xrightarrow{M(g)}
    M^2(c) \xrightarrow{M^2(h)}
    M^3(d) \xrightarrow{M(join_d)}
    M^2(d) \xrightarrow{join_d}
    M(d)\\[1em]
h \circ (g \circ f) &=
    a \xrightarrow{f}
    M(b) \xrightarrow{M(g)}
    M^2(c) \xrightarrow{M^2(h)}
    M^3(d) \xrightarrow{join_{M(d)}}
    M^2(d) \xrightarrow{join_d}
    M(d)\\
\end{aligned}
$$

右端に注目すると, これらが等しくなるためには以下の図式が可換となることが十分であることがわかります.

$$
\begin{CD}
M^3(d)   @>{M(join_d)}>> M^2(d)\\
@V{join_{M(d)}}VV       @VV{join_d}V\\
M^2(d)   @>>{join_d}>    M(d)
\end{CD}
$$

等式で書いておきましょう.

:::{.framed}
**モナドの性質2**

$$
join_a \circ M(join_a) = join_a\circ join_{M(a)}
$$
:::

### 恒等射

恒等射 $\mathrm{Hom}_{\mathcal{C}'}(b, b)$ は圏 $\mathcal{C}$ の中で見ると,

$$ b \to M(b) $$

です. これの存在を保証するために, 自然変換 $return : \mathrm{id} \to M$ を導入しましょう.

$return_b$ が恒等射となるためには $f\in\mathrm{Hom}(b, c),\ g\in\mathrm{Hom}(a, b)$ に対し, 次の条件を満たさなければなりません.

$$
\begin{aligned}
f\circ return_b &= f\\
return_b \circ g &= g
\end{aligned}
$$

書き下してみます.

$$
\begin{aligned}
f = f\circ return_b &=
    b \xrightarrow{return_b}
    M(b) \xrightarrow{M(f)}
    M^2(c) \xrightarrow{join_c}
    M(c)\\
g = return_a\circ g &=
    a \xrightarrow{g}
    M(b) \xrightarrow{M(return_b)}
    M^2(b) \xrightarrow{join_b}
    M(b)
\end{aligned}
$$

以下の可換図式(図式が可換であることは$return$が自然変換であることからわかる)を見ると,

$$
\begin{CD}
b @>{f}>> M(c)\\
@V{return_b}VV @VV{return_{M(c)}}V\\
M(b) @>{M(f)}>> M^2(c) @>{join_c}>> M(c)
\end{CD}
$$

ひとつめの等式が成り立つためには

$$ join_c \circ return_{M(c)} = \mathrm{id}_{M(c)}$$

が成り立つことが十分であることがわかります.

またふたつめの等式が成り立つためには,

$$ join_b \circ M(return_b) = \mathrm{id}_{M(b)}$$

が成り立つことが十分ですね.

まとめると,

:::{.framed}
**モナドの性質3**

$$
join_a \circ return_{M(a)} = join_a\circ M(return_a) = \mathrm{id}_{M(a)}
$$
:::

## モナド則との比較

圏 $\mathcal{C}'$ がちゃんと圏であるために, モナド $M$ が備えてほしい性質を考えてきました.

これらの性質と, はじめに挙げたモナド則を見比べてみましょう.

$$
\begin{aligned}
join_{M(b)}\circ M^2(f) &= M(f)\circ join_a\\[1em]
join_a \circ M(join_a) &= join_a\circ join_{M(a)}\\[1em]
join_a \circ return_{M(a)} &= join_a\circ M(return_a) = \mathrm{id}_{M(a)}
\end{aligned}
$$

```haskell
join . fmap (fmap f) == fmap f . join
join . fmap join     == join . join
join . return        == join . fmap return == id
```

$M(f)$ が `fmap f` と表記されることに注意すれば, 同一の条件を表していることが見て取れます.

以上により, モナド則はとっても自然(圏論用語ではなく人間の感覚として)な条件であることが明らかになりました.
