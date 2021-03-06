---
title: なぜベイズ推論は過学習しづらいのか？
published: 2019-11-17
tags: ベイズ推論, 機械学習
mathjax: on
toc: on
---

ベイズ推論は過学習しづらいと言われます. その理由を, MAP推定が正則化項付きの最尤推定であることを通して理解します.

<!--more-->

{toc}

## 最尤推定は過学習しやすい

最尤推定が過学習しやすいことを2つの例を通して見ていきます.

### ベルヌーイ分布

1枚のコインを投げて表が出る確率を推定することを考えます. つまりコイン $x$ はベルヌーイ分布 $\mathrm{Bern}(x \mid \mu)$ に従うと仮定して, パラメータ $\mu$ を最尤推定します.

データ $X = \{x_1, \ldots, x_N\}$ に対する尤度関数は

$$
p(X \mid \mu) = \prod_{i=1}^N p(x_i \mid \mu) = \prod_{i=1}^N \mu^{x_i}(1-\mu)^{1-x_i}
$$

となります.

ここで, $X = \{1, 1, 1\}$ というデータが得られたとしましょう. つまりコインを3回投げて3回とも表が出た状況を考えているということです. すると尤度関数は以下のように計算できます.

$$
p(X \mid \mu) = \mu^3
$$

これを最大化する $\mu$ を $0 \leq \mu \leq 1$ の範囲で求めると, $\mu = 1$ が得られます. つまりこのコインは必ず表が出ると推論されたわけです. これは明らかに過学習していますね.


### 多項式回帰

次は多項式回帰を最尤推定の枠組みで考えてみます. 2次元のデータ $(x, y)$ はある多項式関数 $f(x,w)$ に沿って分布しているとします. しかしぴったり $y = f(x, w)$ という関係にあると仮定するのは無理があるので, ガウス分布に従うノイズ $\epsilon$ が以下のように加わっているとします.

$$
y = f(x, w) + \epsilon
$$

ただし $f$ と $\epsilon$ はそれぞれ以下のような多項式と確率変数です.

$$
\begin{aligned}
f(x, w) &= w_nx^n + w_{n-1}x^{n-1} + \cdots + w_0 \\
p(\epsilon \mid \beta) &= \mathcal{N}(\epsilon \mid 0, \beta^{-1})
\end{aligned}
$$

$y$ は $\epsilon$ を $f(x, w)$ だけ平行移動させたものと見れるので, $y$ の分布は以下の式で与えられることになります.

$$
p(y \mid x, w, \beta) = \mathcal{N}(y \mid f(x, w), \beta^{-1})
$$

これで $x$ に対して $y$ を出力するモデルが設定できたので, データ $\{(x_i, y_i)\}_{i=1}^N$ に対して最尤推定を行ってみましょう. まずは尤度関数を計算します. ただし $X = \{x_i\}_{i=1}^N$, $Y = \{y_i\}_{i=1}^N$ です.

$$
\begin{aligned}
p(Y \mid X, w, \beta) &= \prod_{i = 1}^N \mathcal{N}(y_i \mid f(x_i, w), \beta^{-1}) \\
    &= \left(\frac{\beta}{2\pi}\right)^{\frac{N}{2}}
        \prod_{i=1}^N \mathrm{exp}\left(-\frac{(y_i - f(x_i, w))^2 \beta}{2}\right)
\end{aligned}
$$

これを最大化するようなパラメータを決定していくわけですが, 計算機でも扱いやすいように対数尤度を考えます. さらに符号を反転させて最小化を考えることとします.

$$
-\log p(Y \mid X, w, \beta) = \frac{\beta}{2}\sum_{i=1}^N (y_i - f(x_i, w))^2 - \frac{N}{2}\log\beta + \mathrm{const.}
$$

まずは $w$ に関する最小化を考えます. ガウス分布の精度パラメータ $\beta$ は正数なので, 上のマイナス対数尤度を最小化するには $\sum (y_i - f(x_i, w))^2$ が最小にならなければなりません. ここだけ見ると, 最小二乗法による多項式回帰と同じですね. これで定まる $w$ の推定値を $\hat{w}$ としましょう.

次に $\beta$ を最適化します. マイナス対数尤度の $w$ を $\hat{w}$ に置き換えたものを $\beta$ で微分し, 最適解を計算すると, 以下のように求まります.

$$
\frac{1}{\hat{\beta}} = \frac{1}{N}\sum_{i = 1}^N (y_i - f(x_i, w))^2
$$

さて, この推論がどのように過学習を引き起こすのかを見ていきましょう.

これまでサイズが $N$ であるようなデータセットを $n$ 次多項式で近似することを考えていました. では $n = N - 1$ とするとどうなるでしょうか. この場合, 変数 $w_0, \ldots, w_n$ の数とデータの数が一致するため,

$$
\sum_{i = 1}^N (y_i - f(x_i, w))^2 = 0
$$

が解を持ちます. つまり $w$ を完璧に最適化することができるということです.

すると精度パラメータ $\beta$ の推定値は $\hat{\beta} = \infty$ となってしまいます. これは明らかに過学習ですね.


## 過学習を抑える

上で見てきたように, 最尤推定は簡単に過学習を引き起こしてしまいます. それを抑制するための手法として, 正則化というものがあります.

上の多項式近似を例にすると, マイナス尤度関数そのものを最小化するのではなく, 次のような正則化項を含めたものを最小化します.

$$
\frac{\beta}{2}\sum_{i=1}^N (y_i - f(x_i, w))^2 - \frac{N}{2}\log\beta + \lambda \|w\|^2
$$

最後の $\lambda \|w\|^2$ が正則化項です. 全体が小さくなるためには, $\|w\|$ があまり大きくなれません. つまり $w$ の取りうる値の範囲を制限しているということです. これにより, 学習データにぴったり適合してしまうことを防ぐことができます.

## ベイズ推論

さて, やっとベイズ推論について説明できます. これまでいかに過学習が起き, それを抑制するかを見てきました. ここからは, MAP推定が正則化項付きの最尤推定であることを確認していきます.

ここでも多項式近似を例にとります. ただしベイズ推論を行うので, パラメータ $w$ に事前分布を導入します. $\beta$ は簡略化のため事前分布は設定しません.

$$
p(w \mid \lambda) = \mathcal{N}(w \mid 0, \lambda^{-1} I) = \left(\frac{\lambda}{2\pi}\right)^{\frac{n+1}{2}}\exp\left(-\frac{\lambda}{2} \|w\|^2\right)
$$

尤度関数はすでに導入済みなので, 事後分布を計算しましょう.

$$
\begin{aligned}
p(w \mid X, Y, \lambda, \beta) &= p(Y \mid X, w, \beta)p(w \mid \lambda) \\
    &= \prod_{i=1}^N \mathcal{N}(y_i \mid f(x_i, w), \beta)\mathcal{N}(w \mid 0, \lambda^{-1} I)
\end{aligned}
$$

対数をとって計算を進めてみます.

$$
\begin{aligned}
&\log p(w \mid X, Y, \lambda, \beta) \\
= &-\frac{\beta}{2} \sum_{i=1}^N (y_i - f(x_i, w))^2
    + \frac{N}{2}\log\beta
    - \frac{\lambda}{2}\|w\|^2
    + \frac{n+1}{2}\log\lambda
    + \mathrm{const.}
\end{aligned}
$$

ここでMAP推定 (事後確率を最大化するパラメータを推定値とする手法) を考えてみましょう. $w$ を最適化することを考えると, 本質的には以下の式を最大化することになります.

$$
-\frac{\beta}{2}\sum_{i=1}^N (y_i - f(x_i, w))^2 + \frac{N}{2}\log\beta - \frac{\lambda}{2} \|w\|^2
$$

これはまさに, 先に出てきた正則化項付きの最尤推定そのものです.

## まとめ
最尤推定では, 正則化項を付け加えることで過学習を避けました. $\|w\|$ があまり大きくならないようにすることで, $w$ の動ける範囲を制限するというアイデアです.

一方ベイズ推論では事前分布によって $w$ の範囲を狭めています.

モデルの表現力を削ぐというアイデアは共通していて, 且つ正則化項と事前分布の選び方によってはこれらは数式の上でも一致します.

以上の理由により, ベイズ推論は過学習が起きづらいのでした.
