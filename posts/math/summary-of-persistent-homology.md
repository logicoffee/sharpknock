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

#### 諸定義

**multiset**: 重複度(multiplicity)も考慮した集合のことです.

**パーシステント図**: 実数 $\mathbb{R}$ を添え字集合とするフィルトレーション $\{X_\alpha\}_{\alpha\in\mathbb{R}}$, つまり $\alpha < \alpha'$ に対して $X_\alpha \subset X_{\alpha'}$ が成り立つような位相空間の族を考えます. $k$ 次元の穴 $\sigma$ が $X_\alpha$ で発生し, $X_{\alpha'}$ で消滅するようなものであるとき, $(\alpha, \alpha')\in\mathbb{R}^2$ を $p_\sigma$ と表すことにします($p_\sigma$ という記法はこの記事特有のものであることに注意).

フィルトレーション内に現れる全ての $k$ 次元の穴 $\sigma$ に対する, $p_\sigma$ の集合 $\{p_\sigma\}_\sigma$ を $k$ 次パーシステント図といいます. ただしこの集合は重複度(multiplicity)も考慮した, multiset と呼ばれるものです. 例えば $X_\alpha$ で発生し, $X_{\alpha'}$ で消滅するような $k$ 次元の穴が丁度2つ存在した場合, $(\alpha, \alpha')$ の重複度は $2$ です.

論文によっては対角線の各点を重複度 $\infty$ として加えたものを考えている場合もあります.

定義から, パーシステント図の各点は対角線よりも上側に存在することがわかります. また, 消滅時刻と発生時刻の差 $\alpha'-\alpha$ を $\sigma$ の persistence と言います.

#### 付随する概念

multiset 間の写像 multi-bijection とは, 重複度も考慮した全単射のことです. 

対角線上の点を重複度 $\infty$ として加えたパーシステント図同士の p-Wasserstein distance とは, 以下で定義される距離です.

$$
W_p(D, D') = \inf_{\gamma:D\to D'}\left(\sum_{u\in B}\|u-\gamma(u)\|^p_\infty\right)^{1/p}
$$

ただし $\gamma$ は multi-bijection 全体を走ります.


#### 長所

可視性に優れています. 

#### 短所

代数的な操作ができません. 例えば2つのパーシステント図の和を考えることができません.

p-Wasserstein distance の計算はとても大変です.

### Barcodes

### Persistence Landscapes

### Persistence Indicator Functions

### Persistence Images

### Persistence Weighted Gaussian Kernel
