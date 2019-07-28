---
title:
published:
tags: PersistentHomology
toc: on
mathjax: on
---

パーシステントホモロジーを利用したクラスタリングを Python で実装してみました.

<!--more-->

{toc}

以下の記事で, パーシステントホモロジーを使ったクラスタリング方法のアイデアを解説しました.

[パーシステントホモロジーでクラスタリング](/posts/math/clustering-using-persistent-homology.html)

本記事ではこのクラスタリングアルゴリズムを Python で実装してみます. 上の記事と同様に [Persistence-Based Clustering in Riemannian Manifolds](https://hal.inria.fr/inria-00389390/document) という論文を参考にしていきます.
