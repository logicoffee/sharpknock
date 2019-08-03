---
title: パーシステントホモロジーを使ったクラスタリングを実装した
published: 2019-08-03
tags: PersistentHomology, Clustering
---

パーシステントホモロジーを利用したクラスタリングを Python で実装してみました.

<!--more-->

以下の記事で, パーシステントホモロジーを使ったクラスタリング方法のアイデアを解説しました.

[パーシステントホモロジーでクラスタリング](/posts/math/clustering-using-persistent-homology.html)

このクラスタリングアルゴリズムを Python で実装してみました. 上の記事と同様に [Persistence-Based Clustering in Riemannian Manifolds](https://hal.inria.fr/inria-00389390/document) という論文を参考にしていきます.

ソースコードは [Github](https://github.com/logicoffee/pluster) にあります.

2つの2次元正規分布を混合した分布からサンプルした100個の点をクラスタリングした結果が以下です. 2つの大きなクラスタを出力できていることがわかりますね.

![](/images/pluster.png)


まだまだアルゴリズムを十分に理解しきれていない点もありますが, 理解でき次第記事にしていきたいと思います.
