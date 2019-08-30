---
title: パーシステントホモロジーの要約量
published: 2019-08-25
tags: PersistentHomology
toc: on
mathjax: on
---

パーシステントホモロジーを要約するものの代表はパーシステント図です. しかしこれ以外にも多様な要約量が考案されています. 本記事ではそれらの定義を延べ, 長所や短所を解説します.

<!--more-->

{toc}

## パーシステントホモロジーとは

パーシステントホモロジーとは, トポロジカルデータ分析(TDA)で使われている数学的な道具です. データセット(Point Cloud)の位相的な特徴を捉えることで, ノイズに強いデータ分析が可能です.

もう少し詳しい解説は以下の記事に書いています.

- [ねこでも分かるホモロジー](/posts/math/what-is-homology.html)
- [ねこでも分かるパーシステントホモロジー](/posts/math/persistent-homology.html)


## 要約量

### Persistence Diagrams

日本語の文献ではパーシステント図と表記されます.

#### 初出

この論文が初出だってのは見つかりませんでした.

[2007年のこの論文](https://link.springer.com/content/pdf/10.1007/s00454-006-1276-5.pdf)に, [2002年のこの論文](https://link.springer.com/content/pdf/10.1007/s00454-002-2885-2.pdf)で導入されたと書かれているのは見つけましたが, パーシステント図自体は登場していません. 似たような概念は出てきていますが.

#### 諸定義

**multiset**: 各点の重複度(multiplicity)も考慮した集合のことです. 例えば $\{a, a, a, b, b\}$ などです.

**multi-bijection**: multiset 間の全単射です. ただし重複度も考慮します. 例えば $\{a, a, a, b, b\}$ と $\{x, y, y\}$ の間には, 通常の集合としては全単射が存在しますが, multi-bijection は存在しません.

**パーシステント図**: フィルトレーションから生じる multiset のことです. もう少し詳しく説明しましょう. 実数 $\mathbb{R}$ を添え字集合とするフィルトレーション $\{X_\alpha\}_{\alpha\in\mathbb{R}}$, つまり $\alpha < \alpha'$ に対して $X_\alpha \subset X_{\alpha'}$ が成り立つような位相空間の族を考えます. 添字 $\alpha$ を時刻だと思えば, 時間が進むにつれて大きくなる位相空間を想像してもらえばいいかと思います.

位相空間の時間発展に伴って $k$ 次元の穴が出来たり消えたりします. $k$ 次元の穴 $\sigma$ の発生時刻を $b_\sigma$, 消滅時刻を $d_\sigma$ と表すことにします. つまり $\sigma$ は $X_{b_\sigma}$ で発生し $X_{d_\sigma}$ で消滅するようなものであるということです. $d_\sigma - b_\sigma$ のことを $\sigma$ の persistence と言います.

$k$ 次パーシステント図とは, フィルトレーション内に現れる $k$ 次元の穴全てに対して考えた multiset $\{(b_\sigma, d_\sigma)\}_\sigma$ のことです. ただし重複度まで考慮していることに注意です. 例えば $X_\alpha$ で発生し, $X_{\alpha'}$ で消滅するような $k$ 次元の穴が丁度2つ存在した場合, $(\alpha, \alpha')$ の重複度は $2$ です. また $b_\sigma < d_\sigma$ ですから, パーシステント図の各点は対角線よりも上側に存在することがわかります.

論文や場面によっては対角線の各点を重複度 $\infty$ として加えたものを考えている場合もあります.

以下がパーシステント図の例です([homcloud](https://www.wpi-aimr.tohoku.ac.jp/hiraoka_labo/homcloud/)というソフトウェアを使って出力しています).

![](/images/pd_example.png)

**p-Wasserstein distance**: 対角線上の点を重複度 $\infty$ として加えたパーシステント図同士の p-Wasserstein distance とは, 以下で定義される距離です.

$$
W_p(D, D') = \inf_{\gamma:D\to D'}\left(\sum_{u\in D}\|u-\gamma(u)\|^p_\infty\right)^{1/p}
$$

ただし $\gamma$ は multi-bijection 全体を走ります.

**bottleneck distance**: 対角線上の点を重複度 $\infty$ として加えたパーシステント図同士の bottleneck distance とは $W_\infty$ のことです.

$$
W_\infty(D, D') = \inf_{\gamma:D\to D'}\sup_{u\in D}\|u-\gamma(u)\|_\infty
$$

#### 長所

データのノイズに対する安定性があります. [この論文](https://geometry.stanford.edu/papers/ccggo-ppmd-09/ccggo-ppmd-09.pdf)では, 関数の sublevel sets に由来するするパーシステント図の安定性について論じています. ざっくり言うと

$$
\|f-g\|_\infty < \varepsilon \Longrightarrow W_\infty(D_f, D_g) < c\varepsilon
$$

ということです. ただし $c$ は条件によって変化します.

[こちらの論文](https://arxiv.org/pdf/1207.3885.pdf)には, 有限距離空間の Rips complex や Cech comlex などに由来するパーシステント図の安定性について論じています. ざっくり言うと

$$
W_\infty(D_X, D_Y) \leq 2\mathrm{d_{GH}}(X, Y)
$$

ということです. ただし $\mathrm{d_{GH}}$ は Gromov-Housdorff distance です.

他に利点を挙げるとすれば, 可視性に優れていることでしょうか. 元のデータがどんな高次元に住んでいようと, パーシステント図は2次元平面内に表現できます.

#### 短所

Wasserstein distance は全単射全体に関して $\inf$ を考えています. つまり並べ替え全体を考えているということです. したがってパーシステント図を構成する点の数が増えるに従って, 計算量が爆発的に増加します.

Wasserstein distance によってパーシステント図全体には距離空間の構造が入ります. しかし統計や機械学習などを適用しようと思ったらそれだけでは足りません. 例えば2つのパーシステント図の平均などを考えることができません. この欠点を解決するために, Persistence Landscaped を始めとする他の要約量が編み出されました.


### Barcodes

#### 諸定義

**Barcode**: パーシステント図は点の集合 $\{(b_\sigma, d_\sigma)\}_\sigma$ でしたが, Barcode は区間の集合 $\{[b_\sigma, d_\sigma]\}_\sigma$ です.


#### 長所

パーシステントホモロジーはフィルトレーションにおけるホモロジー類のライフサイクルを捉えるものです. フィルトレーションというのは位相空間の増大列ですから, $0$ 次ホモロジー類である連結成分は必ずひとつ以上残ります. つまりこのホモロジー類は消滅時刻を持ちません. したがって, このホモロジー類に対応するパーシステント図上の点も存在しないことになります. しかし Barcode ならば右半開区間 $[b_\sigma, \infty)$ を考えることにより, このホモロジー類も扱うことができます. 

またパーシステント図と同様に可視化に優れています. 以前書いた[こちらの記事](/posts/math/clustering-using-persistent-homology.html)のように, 息の長いホモロジー類の個数が数えやすいため, クラスタリングもしやすいです.

#### 短所

パーシステント図と同様に2つの Barcodes の平均を求めるようなことはできません.


### Persistence Landscapes

#### 初出

[Statistical Topological Data Analysis using Persistence Landscapes](http://www.jmlr.org/papers/volume16/bubenik15a/bubenik15a.pdf)

#### 諸定義


### Persistence Indicator Functions

### Persistence Images

### Persistence Weighted Gaussian Kernel
