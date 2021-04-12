---
title: デルタ法
published: 2021-04-09
tags: delta method
mathjax: on
toc: on
---

A/B テストにおいて, ランダム化単位と分析単位が異なることはよくあります. そのような場合はデータが iid ではないため中心極限定理が使えず, 正規分布に帰着できません. その悩みを解決してくれるのがデルタ法です.

<!--more-->

{toc}

## 参考文献

本記事の内容は以下の論文をもとに書きました.

- [Applying the Delta Method in Metric Analytics: A Practical Guide with Novel Ideas](https://arxiv.org/pdf/1803.06336.pdf)

デルタ法を知ったきっかけは以下の本です.

- [A/Bテスト実践ガイド](https://www.kadokawa.co.jp/product/302101000901/)

## 問題設定
A/B テストにおけるパターンの振り分けを, ユーザーごとランダムに行ったとしましょう. つまりランダム化単位がユーザーです.

まずは分析単位もユーザーである場合を考えてみます. 例えばユーザー 1 人あたりの購買金額のような指標を考えているような場合です. 各ユーザーは独立ですし, ユーザーごとの購買金額が同分布であると仮定できるのなら, サンプルは iid です. 中心極限定理により標本平均は正規分布に近似でき, 検定を行ったり信頼区間を計算したりできます.

では分析単位がユーザーでない場合はどうなるでしょうか. 例えば CTR のようなページ単位の指標を扱うような場合です. 1 人のユーザーが何度もページにアクセスすることもあるため, サンプルは iid ではありません. したがって中心極限定理を使うことができず, 検定や信頼区間の計算ができません.

後者のようにランダム化単位と分析単位が揃ってない場合でも, デルタ法を使うことで標本平均を正規分布に近似させることができるのです.

## 概念の整理
以下で登場する 2 つの概念について整理しておきます.

### 分布収束
:::{.framed}
確率変数列 $\{X_n\}_n$ が確率変数 $X$ へ分布収束するとは, 以下が成り立つことをいう.

$$
\forall x. \lim_{n \to \infty} P(X_n \leq x) = P(X \leq x)
$$
:::

つまり累積分布関数の列として各点収束するという意味ですね.

### ランダウの記号
アルゴリズムの計算量や関数の振る舞いに対して使われるランダウの記号ですが, 確率変数列についても定義されています.

:::{.framed}
**関数のランダウの記号**

$$
f(x) = O(g(x)) \quad \mathrm{as} \ x \to \infty \\
\overset{\mathrm{def}}{\Longleftrightarrow} \quad \exists x_0,\ \exists M > 0,\ x > x_0 \Longrightarrow |f(x)| < M|g(x)|
$$

<br />

$$
f(x) = O(g(x)) \quad \mathrm{as} \ x \to a \\
\overset{\mathrm{def}}{\Longleftrightarrow} \quad \exists \delta > 0,\ \exists M > 0,\ |x - x \Longrightarrow |f(x)| < M|g(x)|
$$
:::

:::{.framed}
**確率変数列のランダウの記号**

$$
X_n = O(\epsilon_n) \\
\overset{\mathrm{def}}{\Longleftrightarrow} \quad \forall \delta > 0, \ \exists M > 0, \ \exists n_0, \ n > n_0 \Longrightarrow P(|X_n| \leq M|\epsilon_n|) \geq 1 - \delta
$$
:::

## デルタ法
確率変数列 $\{T_n\}_n$ がある定数 $\theta$ に対し以下をみたすとします. ただし矢印は分布収束を表します.

$$
\sqrt{n}(T_n - \theta) \overset{d}{\longrightarrow} n(0, 1)
$$

このとき以下が成り立ちます.

$$
T_n - \theta = O(1/\sqrt{n})
$$

任意の連続関数 $\phi$ に対して

$$
\phi(x) - \phi(\theta) = \phi'(\theta)(x - \theta) + O((x - \theta)^2)
$$

が成り立ちますから, 確率変数列 $\{T_n\}_n$ に対しても同様のことが成り立ちます.

$$
\phi(T_n) - \phi(\theta) = \phi'(\theta)(T_n - \theta) + O((T_n - \theta)^2)
$$

これにより, 次の分布収束を得ます.

$$
\sqrt{n}(\phi(T_n) - \phi(\theta)) \overset{d}{\longrightarrow} N(0, \phi'(\theta)^2)
$$

## 実践例

問題設定で, ランダム化単位と分析単位が異なる場合にデルタ法が解決してくれると書きました. それを見ていきましょう.

ユーザーごとにランダム化を行い, ページ単位の指標である CTR を分析対象とします.

ユーザー $n$ のアクセス数を $X_n$ とすれば, 各 $X_n$ 同士は独立です. さらに同分布と仮定しましょう. これで $\{X_n\}_n$ は iid なサンプルとなりましたから, 標本平均 $\overline{X}_n = \sum_n X_n / n$ は中心極限定理により正規分布に収束します.

ユーザーごとのクリック数 $Y_n$ も iid であると仮定すれば, $\overline{Y}_n = \sum_n Y_n / n$ も正規分布に収束します.

ここで, $T_n = (\overline{X}_n, \overline{Y}_n), \theta = (\mu_x, \mu_y), \phi(x, y) = y / x$ と定義することで, デルタ法が適用できる状態になります. $\phi(T_n) - \phi(\theta)$ は $\nabla\phi(\theta) \cdot (T_n - \theta)$ で近似できますから, 各記号を書き下すことで以下の近似が得られます.

$$
\frac{\overline{Y}_n}{\overline{X}_n} - \frac{\mu_y}{\mu_x}
\approx \frac{1}{\mu_y}(\overline{Y}_n - \mu_y) - \frac{\mu_y}{\mu_x^2}(\overline{X}_n - \mu_x)
$$

$W_n = Y_n / \mu_x - \mu_yX_n/\mu_x^2$ とすれば $\{W_n\}_n$ は iid であり, 右辺は $\sum_n W_n / n$ という形をしています. つまり右辺に対しては中心極限定理が適用でき, 信頼区間の計算も可能です. 具体的には, 信頼区間は以下のように表されます.

$$
\frac{\overline{X}}{\overline{Y}} - 1 \pm \frac{z_{\alpha/2}}{\sqrt{n}\overline{X}}\sqrt{s_y^2 - 2\frac{\overline{X}}{\overline{Y}}s_{xy} + \frac{\overline{X}^2}{\overline{Y}^2}s_x^2}
$$
