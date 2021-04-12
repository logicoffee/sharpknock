---
title: ベイズ推論入門
published: 2021-02-16
tags: ベイズ推論, 統計
mathjax: on
toc: on
---

ベイズ推論がどういうものなのか, 数式を使いながら具体例を交えて解説しました.

<!--more-->

{toc}

## 準備

この記事を読むためには条件付き確率に対する理解が必要です. この分野は学習指導要領に含まれていた人もいればそうでない人もいますね.

### 定義

事象 $A$ の起こる確率を $P(A)$ と書きます.

:::{.framed}
**定義** 事象 $A$ が起きたことを既知とした, 事象 $B$ の起こる確率を $P(B\mid A)$ と書き, これは次のように計算されます.

$$
P(B\mid A) = \frac{P(A \cap B)}{P(A)}
$$
:::

### 具体例

具体例で考えてみましょう.

:::{.framed}
**問題** 赤玉3個と白玉2個が入った箱Xと, 赤玉1個と白玉3個が入った箱Yがある. サイコロをふって3の倍数が出たら箱Xから玉をひとつ取り出し, 3の倍数以外が出たら箱Yから玉をひとつ取り出す. 今サイコロふったところ3が出た. 取り出す玉が赤である確率を求めよ.
:::

サイコロの目は3の倍数なので箱Xから玉を取り出すことになり, 直感的に $3/5$ とわかりますが, 定義通りにも計算してみましょう.

$$
\begin{aligned}
P(\text{赤を取り出す} \mid \text{3が出る}) &= \frac{P(\text{赤を取り出す} \cap \text{3が出る})}{P(\text{3が出る})} \\[0.6em]
&= \frac{3/5 \cdot 1/6}{1/6} \\[0.4em]
&= \frac{3}{5}
\end{aligned}
$$

結局 $1/6$ が相殺されるので, 箱Xから赤玉を取り出す確率だけが残ります. 次の例にいきます.

:::{.framed}
**問題** 前問と同じ設定で, 取り出した玉が赤色であった場合に, サイコロの目が3の倍数である確率を求めよ.
:::

さっきの問題と違って, 既知の事象と確率計算の対象となる事象が逆になっていますね. 時系列を逆に辿ることになるため直感的には確率は分かりません. 定義通りに計算していきましょう.

$$
\begin{aligned}
P(\text{3の倍数が出る} \mid \text{赤を取り出す}) &= \frac{P(\text{3の倍数が出る} \cap \text{赤を取り出す})}{P(\text{赤を取り出す})} \\[0.6em]
&= \frac{1/3 \cdot 3/5}{1/3 \cdot 3/5 + 2/3 \cdot 1/4} \\[0.4em]
&= \frac{6}{11}
\end{aligned}
$$

### 記号について

機械学習などの本でよく使われる記法について述べておきます.

#### 記号の乱用

確率変数 $x$, $y$ の確率密度関数をそれぞれ $p(x)$, $p(y)$ のように書き, これらの同時確率密度関数は $p(x, y)$ のように表します.

これらは実は2つの意味で記号の乱用です.

ひとつめの乱用は, 本来以下のように確率変数と関数の引数は別の記号で表されるのに対し, 上では同じ記号を用いている点です.

$$
P(X \leq x) = \int_{-\infty}^x p(x) dx
$$

ふたつめの乱用は, $p(x)$ と $p(y)$ は別の関数であるにも関わらず同じ $p$ を使っている点です. これは $P(X = x)$ や $P(Y = y)$ のようなノリで使っています.

この記法はそれなりに一般的であると思われるることと, ときには乱用しないと記号が足りないこともあるため, この記事でもこのように書くこととします.

#### 条件付き確率密度関数

縦棒で条件付き確率を表す記法は確率密度関数にも適用されます.

上で述べたような記法のもとで $p(x \mid y)$ を

$$
p(x \mid y) = \frac{p(x, y)}{p(y)}
$$

と定義します. イメージとしては以下のようなものを計算していることになります. 

$$
P(X = x \mid Y = y) = \frac{P(X = x, Y = y)}{P(Y = y)}
$$

## 基準率の錯誤

ベイズ推論の枠組みを説明するために以下のような問題を考えてみます.

:::{.framed}
**問題** とある感染症には全人口の 1% が感染している. 新たに開発された検査キットは 80% の信頼性を持つ. つまり正しく陽性/陰性と診断される確率は 80% で, 偽陽性/偽陰性と診断される確率は 20% である. この検査キットを使って陽性診断を下された人が, 真に陽性である確率はいくらか？
:::

もし自分が陽性診断を受けた場合, 80% の確率で感染しているんだと思ってしまいます. しかし答えは 80% ではなく, 約 3.88% です.

よく見るとこれはサイコロと玉の問題と同じ形をしています. サイコロの出目によってシナリオが分岐していたのと同様に, ここでは感染しているかどうかがシナリオを分岐させています.

感染していない人も 20% の確率で偽陽性反応が出ることを考慮して, 条件付き確率の定義に沿って計算してみましょう.

$$
\begin{aligned}
P(\text{感染している} \mid \text{陽性反応が出た}) &= \frac{P(\text{感染している} \cap \text{陽性反応が出た})}{P(\text{陽性反応が出た})} \\[0.6em]
&= \frac{0.01 \cdot 0.8}{0.01 \cdot 0.8 + 0.99 \cdot 0.2} \\[0.6em]
&= \frac{4}{103} \\[0.6em]
&\fallingdotseq 3.88\%
\end{aligned}
$$

感覚的には 80% と答えてしまいそうなこの問題ですが, それは人口の 1% が感染しているという情報を無視して考えてしまっているからです. このような間違いを「基準率の錯誤」などといいます. ここでは全人口に対する感染率である 1% が基準率 (base rate) です. この率がベース (=基準) にあるはずなのに, それを無視して検査キットの信頼性だけを見てしまうのが基準率の錯誤です.

## ベイズ推論

さて本題のベイズ推論です. 上の感染症の問題をベイズ推論という枠組みで考えていきます.

### ベイズ推論の流れ

ベイズ推論に限らず, データ $X$ がパラメータ $\Theta$ をもつような分布から生成されているという状況を考えることはよくあります. 状況を考える, とはいってもそれはただ人間がモデリングしているにすぎないため, 本当のところは分かりませんが.

例えば成人男性の平均身長 $X$ が正規分布 $\mathcal{N}(X \mid \mu, \sigma^2)$ に従うと仮定するのはよくありますね. ここでも条件付き確率の記法が登場しています. 正規分布は平均と分散の2つのパラメータを持ちますから, これらをまとめて $\Theta$ と表しています. 他にも, コインを投げて表が出たら 1, 裏が出たら 0 を対応させるような確率変数 $X$ をベルヌーイ分布 $\mathrm{Bern}(X \mid \mu)$ でモデリングしたりします.

いわゆる頻度論では, 得られたデータをもとに母平均の推定や検定を行いますよね. ベイズ推論においても, 得られたデータをもとにして確率モデルを調べるということ自体に違いはありません. ただそのやり方が少し違うのです.

ベイズ推論でよくあるのは, パラメータ $\Theta$ も確率変数として扱うパターンです. よく頻度論とベイズ統計を二項対立として扱って, 頻度論では固定値として扱うものをベイズ統計では確率変数として扱うなどと説明されたりしますが, 手元のデータから真の分布を探る道具であることには変わりありません. 固定値も, 100% の確率でその値を取るような確率変数としてみなすことができますから, 確率変数として扱うことが本質ではありません. これについては [東京工業大学の渡辺澄夫先生のスライド](http://watanabe-www.math.dis.titech.ac.jp/users/swatanab/bayes000.pdf) が参考になります.

さて, 少し脱線してしまいましたが今目の前にあるは $\Theta$ の分布 $p(\Theta)$ と, $X$ の分布 $p(X \mid \Theta)$ です. $X$ の分布が条件付きになっているのは, 例として挙げた $\mathcal{N}(X \mid \mu, \sigma^2)$ や $\mathrm{Bern}(X \mid \mu)$ と同じです. $X$ の分布はパラメータ $\Theta$ に依存しているのです.

$p(\Theta)$ のことを事前分布と言い, $p(X \mid \Theta)$ を尤度関数と言います. この2つでもって研究対象をモデリングしています.

頻度論における最尤推定では「データ $X$ が得られるためには, パラメータは $\hat{\Theta}$ であるのが尤もらしい」のように考え, 点推定値 $\hat{\Theta}$ を求めます. ベイズ推論では $p(\Theta)$ と $p(X \mid \Theta)$ を使って $p(\Theta \mid X)$ を計算します. この $p(\Theta \mid X)$ を事後分布と言います.

事前分布 $p(\Theta)$ はいわばモデルの初期値です. それと手元のデータ $X$ 使って $p(\Theta \mid X)$ を導出します. $X$ の情報を含んでいるため, $p(\Theta)$ に比べ $p(\Theta \mid X)$ のほうがより現実を反映しているものになっています.

### 感染症の問題

一般論だけでは理解しづらいため, 感染症の問題をベイズ推論を使って考えてみます. まずは問題をモデリングしましょう. 準備するものは事前分布と尤度関数です.

尤度関数のほうが考えやすいためこちらから設定していきましょう. 尤度関数はデータ $X$ を生成する分布のことです. この問題におけるデータとは何でしょうか？ それは診断結果です. 陽性診断というデータを使って, 本当に自分は感染しているんだろうか？ということを推論するわけです. 診断結果は陽性か陰性の2値しかありませんから, コインと同様にベルヌーイ分布を設定します.

陽性診断と陰性診断それぞれに対し 1, 0 を割り当て, これを $X$ と表すことにします. 検査キットの信頼性は 80% ですから, 真に感染している場合は $\mathrm{Bern}(X \mid 0.8)$ という分布になりますし, 真に感染していない場合は $\mathrm{Bern}(X \mid 0.2)$ という分布になります.

これらをまとめて $\mathrm{Bern}(X \mid \mu')$ と書くことにすれば, $\mu'$ は以下のような変数です.

$$
\mu' = \begin{cases}
0.8 & (\text{真に感染しているとき}) \\
0.2 & (\text{真に感染していないとき})
\end{cases}
$$

$\mu'$ は真に感染しているかどうかによって取る値が変わります. つまり $\mu'$ も確率変数だということです. $\mu'$ は尤度関数 $p(X \mid \mu') = \mathrm{Bern}(X \mid \mu')$ のパラメータなので, $\mu'$ の分布は事前分布ですね.

$\mu'$ は2値を取るのでベルヌーイ分布が丁度いいのですが, 1 と 0 ではなく 0.8 と 0.2 を取るのはちょっと面倒です. したがって $\mu' = 0.6\mu + 0.2$ のように変数変換しましょう. すると $\mu$ は 0 と 1 を取るようになります.

$\mu$ は真の陽性か陰性かを表しているので, ベルヌーイ分布 $p(\mu)$ のパラメータは感染率です. 問題設定から感染率は 1% なので, $p(\mu) = \mathrm{Bern}(\mu \mid 0.01)$ となります. これで事前分布の設定もできました.

モデル構造をまとめておきましょう.

- 事前分布
    - $p(\mu) = \mathrm{Bern}(\mu \mid 0.01)$
    - $\mu$ は真の感染状態
    - 感染していれば $\mu = 1$, 感染していなければ $\mu = 0$
- 尤度関数
    - $p(X \mid \mu') = \mathrm{Bern}(X \mid \mu')$
    - $X$ は診断結果
    - 陽性診断であれば $X = 1$, 陰性診断であれば $X = 0$
    - $\mu$ と $\mu'$ は $\mu' = 0.6\mu + 0.2$ という関係にある

ではいよいよ事後分布の計算です. 条件付き確率について一般に以下が成り立ちます.

$$
\begin{aligned}
p(\Theta \mid X) &= \frac{p(X, \Theta)}{p(X)} \\[0.5em]
&= \frac{p(X, \Theta)}{p(\Theta)} \cdot \frac{p(\Theta)}{p(X)} \\[0.5em]
&= \frac{p(X \mid \Theta)p(\Theta)}{p(X)}
\end{aligned}
$$

今陽性診断を下された場合を考えているため $X = 1$ です. つまり $p(\mu \mid X = 1)$ が計算対象です. また, $\mu$ は $0$ と $1$ のみを取りうるので, $p(\mu = 1 \mid X = 0)$ と $p(\mu = 0 \mid X = 1)$ をそれぞれ計算すれば良いことになります.

まずは分子を計算しましょう.

$$
\begin{aligned}
p(\mu = 0 \mid X = 1) \ \text{の分子} &= p(X = 1 \mid \mu' = 0.2)p(\mu = 0) \\
&= \mathrm{Bern}(X = 1 \mid \mu' = 0.2)\mathrm{Bern}(\mu = 0 \mid 0.01) \\
&= 0.2 \cdot 0.99 \\
&= 0.198
\end{aligned}
$$

$$
\begin{aligned}
p(\mu = 1 \mid X = 1) \ \text{の分子} &= p(X = 1 \mid \mu' = 0.8)p(\mu = 1) \\
&= \mathrm{Bern}(X = 1 \mid \mu' = 0.8)\mathrm{Bern}(\mu = 1 \mid 0.01) \\
&= 0.8 \cdot 0.01 \\
&= 0.008
\end{aligned}
$$

これで分子が計算できました. 残るは分母だけ...と思いきや, 実は分母を計算する必要はありません. 全確率は 1 なので, 以下が成り立ちます.

$$
\sum_{\mu = 0, 1} p(\mu \mid X) = p(\mu = 0 \mid X) + p(\mu = 1 \mid X) = 1
$$

これが成り立つように正規化すればいいわけですから, ほしい確率は以下のように計算できます.

$$
\begin{aligned}
p(\mu = 0 \mid X = 1) &= \frac{0.198}{0.198 + 0.008} = \frac{99}{103} \\[0.5em]
p(\mu = 1 \mid X = 1) &= \frac{0.008}{0.198 + 0.008} = \frac{4}{103}
\end{aligned}
$$

これで事後分布が計算できました. 事前分布と事後分布をそれぞれ並べて書いてみましょう.

- 事前分布：$\mathrm{Bern}(\mu \mid 0.01)$
- 事後分布：$\mathrm{Bern}(\mu \mid 4 / 103)$

事前分布は, 陽性診断を受けたというデータがない状態の分布です. 診断結果の知識がない状態では, 感染している確率は $0.01$ です.

事後分布は事前分布に陽性診断が下された事実を反映させたものになっています. 事後分布のパラメータから, 陽性診断を受けた上で感染している確率は $4/103$ だと分かりました. これは条件付き確率の定義通りに計算したものと同じですね.

このようにして, ベイズ推論の枠組みを用いて感染確率を計算することができました.

## さいごに

事前分布と尤度関数を設定し, 得られたデータを事後分布に反映させる, という一連の流れを具体例を通してみてきました. 読んだだけではなかなか理解しづらいところもあると思います. ぜひとも自分で計算してみることをおすすめします.