---
title: パーシステントホモロジーに関する文献リスト
published: 2019-08-10
tags: PersistentHomology
---

パーシステントホモロジーに関連する文献やソフトウェアをまとめました. 随時更新していきます.

<!--more-->

## 書籍

### [タンパク質構造とトポロジー (2013)](https://www.kyoritsu-pub.co.jp/bookdetail/9784320110021)

日本語で書かれたパーシステントホモロジーの教科書はこれだけだと思います. 単体複体の定義, ホモロジーの定義, パーシステントホモロジーの定義と進んで行きます. 代数トポロジーを既に知っている人はあっという間に読めてしまうかもしれません.

## 論文

### [Computing Persistent Homology (2005)](https://link.springer.com/content/pdf/10.1007/s00454-004-1146-y.pdf)

体上のパーシステントホモロジーを計算するアルゴリズムを解説しています. 基底の取り替え, つまり行列の基本変形によって計算が簡単になることを使ってアルゴリズムを導出しています.

### [Persistence-Based Clustering in Riemannian Manifolds (2009)](https://hal.inria.fr/inria-00389390/document)

パーシステントホモロジーを使ったクラスタリングアルゴリズムが紹介されています. 当ブログでも以下の2つの記事でこの論文を取り上げました.

- [パーシステントホモロジーでクラスタリング](/posts/math/clustering-using-persistent-homology.html)
- [パーシステントホモロジーを使ったクラスタリングを実装した](/posts/math/implementationof-persistence-based-clustering.html)

論文の内容は結構重いです. 多様体上でハウスドルフ測度ベースの確率分布を考えています. クラスタを確率密度関数のピークと捉えることで, superlevel set によるパーシステントホモロジーでクラスタを数えています.


### [Proximity of Persistence Modules and their Diagrams (2009)](https://geometry.stanford.edu/papers/ccggo-ppmd-09/ccggo-ppmd-09.pdf)

上の論文でも引用されている論文です. パーシステント図の安定性について延べています.

パーシステント図は一般に加群の列があれば定義できます. 点を太らせていくパーシステントホモロジーも, ホモロジー群の列を経由してパーシステント図を導いていますからね. この加群の列を persistence module と呼び, パーシステント図同士には bottleneck dictance という距離を導入します. persistence module が似通っていれば, bottleneck distance も小さいことを証明しています.

### [Statistical Topological Data Analysis using Persistence Landscapes (2015)](http://www.jmlr.org/papers/volume16/bubenik15a/bubenik15a.pdf)

パーシステントホモロジーを要約するものはパーシステント図とバーコードが有名ですが, ここでは persistence landscape というものを定義しています. パーシステント図やバーコードと異なるのは, persistence landscape は Banach空間に住んでいる点です. したがって2つの landscape の平均などを考えることができます.

### [Topological Data Analysis of Financial Time Series: Landscapes of Crashes (2017)](https://arxiv.org/pdf/1703.04385.pdf)

上の論文で紹介されている persistence landscape が時系列データの解析に有用であることを延べています. ここではアメリカ株式市場の4つの主要な指標を集めたデータに対して persistence landscape のノルムを計算し, ITバブルの崩壊とリーマンショックの手前でノルムの値が上昇することが示されています.


## ソフトウェア

### [Homcloud](https://www.wpi-aimr.tohoku.ac.jp/hiraoka_labo/homcloud/)

パーシステントホモロジーを計算してくれるツールです. 最初に挙げた書籍の著者である平岡先生の研究室で開発されました.
