---
title: ベイズ推論でよく出てくる正規分布の導出公式
published:
tags: 確率分布, ベイズ推論
toc: on
mathjax: on
---

ベイズモデルで正規分布を扱うと数式が複雑になりますよね. そんなときは指数型分布族としての標準形に直すといいかもしれません.

<!--more-->

{toc}


## 指数型分布族とは

確率変数 $x \in \mathbb{R}^m$ を規定する密度関数がパラメータ $\theta \in \mathbb{R}^d$ を用いて以下の形に表現できるとき, この分布を指数型分布族といいます.

$$
p(x\mid\theta) = \frac{1}{Z(\theta)}h(x)e^{\theta^\top \phi(x)}
$$

ただし $Z(\theta)$ は正規化項なので以下の積分で与えられます.

$$
Z(\theta) = \int_{\mathbb{R}^m}h(x)e^{\theta^\top \phi(x)} dx
$$

$\theta$ は標準パラメータ (canonical parameter) と言います.

正規分布や二項分布, ポアソン分布など, よく出てくる分布は指数型分布族です. 本記事ではこの中でも正規分布に絞って, 指数型分布族を扱うことのメリットを解説していきます.

## 普通の正規分布と指数型分布族としての正規分布の関係

普段正規分布を扱うときは, 以下のように平均パラメータと分散パラメータ (両者を併せてモーメントパラメータといいます) を使って表記することが多いかと思います.

$$
\mathcal{N}(x \mid \mu, \sigma^2)
$$

指数型分布族の標準形で表現した場合は, 以下のように標準パラメータ (canonical parameter) を使います.


$$
\mathcal{N}_c(x \mid \xi, \lambda)
$$

Julia の Distributions パッケージに倣い, $\xi, \lambda$ はそれぞれ potential, precision と呼ぶことにしましょう.

これらのパラメータの変換則を1次元と多次元に分けて紹介していきます.

### 1次元

1次元正規分布におけるパラメータの変換則は次のようになります.

$$
\begin{aligned}
&\mathcal{N}(\mu, \sigma^2) = \mathcal{N}_c\left(\frac{\mu}{\sigma^2}, \frac{1}{\sigma^2}\right) \\
&\mathcal{N}_c(\xi, \lambda) = \mathcal{N}\left(\frac{\xi}{\lambda}, \frac{1}{\lambda}\right)
\end{aligned}
$$

次に, 密度関数の数式としてはどのような違いがあるのかを見てみましょう. ただし指数の部分だけを取り出して比べてみます.


$$
\begin{aligned}
&\mathcal{N}(x \mid \mu, \sigma^2): -\frac{(x - \mu)^2}{\sigma^2} \\
&\mathcal{N}_c(x \mid, \xi, \lambda): -\frac{\lambda}{2}x^2 + \xi x
\end{aligned}
$$

とくにベイズ推論では密度関数の全体を計算する必要がないことが多いです. 例えば上に凸な2次関数が $e$ の肩に乗っていたらそれは正規分布だと分かります. したがって上記の形だけ覚えておけば十分です.

### 多次元

多次元正規分布におけるパラメータの変換則は次のようになります.

$$
\begin{aligned}
&\mathcal{N}(\mu, \Sigma) = \mathcal{N}_c\left(\Sigma^{-1}\mu, \Sigma^{-1}\right) \\
&\mathcal{N}_c(\xi, \Lambda) = \mathcal{N}\left(\Lambda^{-1}\xi, \Lambda^{-1}\right)
\end{aligned}
$$

こちらも密度関数の指数部分を比べてみます.

$$
\begin{aligned}
&\mathcal{N}(x \mid \mu, \Sigma): -\frac{1}{2}(x - \mu)^\top \Sigma^{-1} (x - \mu) \\
&\mathcal{N}_c(x \mid \xi, \Lambda): -\frac{1}{2}x^\top\Lambda x + x^\top \xi
\end{aligned}
$$

## 標準形のメリット

ベイズ推論で正規分布を扱う場合, 事後分布は標準形で表したほうがシンプルになります.

しかも Julia の Distributions パッケージでは `NormalCanon` や `MvNormalCanon` という型を用意してくれているため, コードもきれいに保てます.

#### 計算公式

ベイズ推論では複数の密度関数の積を計算することがよくありますが, 2つの正規分布の積は次のように簡単に計算できます.

$$
\mathcal{N}_c(x \mid \xi_0, \Lambda_0)\mathcal{N}_c(x \mid \xi_1, \Lambda_1) = \mathcal{N}_c(x \mid \xi_0 + \xi_1, \Lambda_0 + \Lambda_1)
$$

## ベイズ推論における例

### 行列分解

次のようなモデルを考えます. $X_{u, i}$ はユーザー $u$ のアイテム $i$ に対する評価値を表しています. この行列 $X$ が user factor matrix $W$ と item factor matrix $H$ の積から生成されているというモデルです.

$$
\begin{aligned}
& X \in \mathbb{R}^{U \times I} \\
& W_u \in \mathbb{R}^D \quad (u = 1, \ldots, U) \\
& H_i \in \mathbb{R}^D \quad (i = 1, \ldots, I) \\
& p(X \mid W, H) = \prod_{u, i}\mathcal{N}(X_{u, i} \mid W_u^\top H_i, \lambda^{-1}) \\
& p(W_u) = \mathcal{N}(W_u \mid \mu_u^W, (\Lambda_u^W)^{-1}) \\
& p(H_i) = \mathcal{N}(H_i \mid \mu_i^H, (\Lambda_i^H)^{-1}) \\
\end{aligned}
$$

モデル設計時には平均などの*意味*を考えるため, モーメントパラメータのほうが適していますね.

さて, このモデルを学習するためにギブスサンプリングのアルゴリズムを導出してみましょう.

まずは $W_u$ をサンプルすることを考えます. $p(W_{u, i} \mid W, H)$ の指数部分を取り出すと, 以下のように変形できます. ただし $\sim$ は $W_u$ に関係ない定数部分を除いて等しいことを表します.

$$
\begin{aligned}
-\frac{\lambda}{2}(X_{u, i} - W_u^\top H_i)^2 &\sim -\frac{\lambda}{2}(W_u^\top H_i)^2 +\lambda X_{u, i}\cdot W_u^\top H_i \\
    &= -\frac{1}{2}W_u^\top (\lambda H_i H_i^\top) W_u + W_u^\top (\lambda X_{u, i}H_i)
\end{aligned}
$$

これは $\mathcal{N}_c(W_u \mid \lambda X_{u, i}H_i, \lambda H_i H_i^\top)$ の指数部分と同じです.

今は $u$ を固定して $W_u$ について考えていましたから, 計算すべきは $\prod_i p(X_{u, i} \mid W, H)$ です. したがって $W_u$ の事後分布は以下のように求まります.

$$
\begin{aligned}
&\prod_i\mathcal{N}_c(W_u \mid \lambda X_{u, i}H_i, \lambda H_i H_i^\top) \cdot \mathcal{N}(W_u \mid \mu_u^W, (\Lambda_u^W)^{-1}) \\
&= \mathcal{N}_c(W_u \mid \lambda\sum_i X_{u, i}H_i, \lambda\sum H_i H_i^\top) \mathcal{N}_c(W_u \mid \Lambda\mu_u^W, \Lambda_u^W) \\
&= \mathcal{N}_c(W_u \mid \Lambda\mu_u^W + \lambda\sum_i X_{u, i}H_i, \Lambda_u^W + \lambda\sum H_i H_i^\top)
\end{aligned}
$$
