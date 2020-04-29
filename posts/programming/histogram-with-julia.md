---
title: Julia でヒストグラムを描く(Mac)
published: 2020-04-29
toc: on
tags: Julia
---

Julia でヒストグラムを作成する方法をメモりました.

<!--more-->

{toc}

## 環境

- macOS: 10.15.2
- Julia: 1.4.1

## 登場人物

特にここを理解しなくともヒストグラムの描画はできます (そもそも私もよくわかってません). お急ぎの方は次のセクションへお進みください.

ヒストグラムを描画する上で (直接的/間接的関係なく) 使うのは以下のものたちです.

- GR Framework
- GR.jl
- GKSTerm
- StatsBase.jl
- Plots.jl

GR Framework の説明は [https://gr.readthedocs.io](https://gr.readthedocs.io/) に以下のように書かれています.

> GR is a universal framework for cross-platform visualization applications.

なんか図を描くためのフレームワークっぽいですね.

GR.jl はこのフレームワークのインターフェースを提供している Julia パッケージということでしょうかね.

そして GKSTerm が直接画像の生成を担っているソフトウェアです. GR.jl を Mac にインストールすると, GKSTerm も自動的にインストールされるみたいです.

上のページには以下のような図も載っていました. まーよくわかってなくてもグラフは描けます.

![](/images/gr_framework.png)


以下の手順で直接扱うのが StatsBase.jl と Plots.jl です. StatsBase はデータの集計をするためのパッケージで, Plots はグラフ描画に使います.

Plots はグラフ描画ソフトウェア ( = バックエンド) を抽象化して扱うことができ, バックエンドとして GR を選ぶことも, PyPlot を選ぶこともできるみたいです. デフォルトでは GR が使われます.

## 手順

### パッケージのインストール

StatsBase と Plots をインストールします. ターミナルで Julia の REPL を起動します. そこで `]` を押すと, プロンプトが以下のように変わるはずです.

```
(@v1.4) pkg>
```

ここで `add StatsBase` とすれば StatsBase がインストールできます. Plots をインストールすることで依存関係にある GR もインストールされます.

### 集計

ヒストグラムを作成にはデータの集計がつきものです. データを集計するには以下のように StatsBase を使います.

```julia
using StatsBase

data = [1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
h = fit(Histogram, data, [1, 2, 3, 4, 5])
```

`h` は `Histogram` 型の値なのですが, 実質的には `[3, 2, 3, 4]` という配列が作られているという理解でよいかと思います.

配列 `[1, 2, 3, 4, 5]` により, 「1以上2未満」「2以上3未満」「3以上4未満」「4以上5未満」という4区画に分けて集計することを指定しています.

ただしこんな配列いちいち書いてられないので, StepRange を使って以下のように書くのが良いでしょう.

```julia
h = fit(Histogram, data, 1:1:5)
```

また, 「1以上2未満」ではなく「1より大きく2以下」という区分にしたい場合は `closed` を指定すればOKです.

```julia
fit(Histogram, data, 1:1:5, closed = :left)  # 1以上2未満 (default)
fit(Histogram, data, 1:1:5, closed = :right)  # 1より大きく2以下
```

### 描画

描画には次のように Plots パッケージを使います. とくに説明がなくともなにをやっているかは分かるかと思います.

```julia
using Plots

plot(h, title = "my histogram", label = "number of values", ylim = (0, 4))
```

すると以下のようなヒストグラムが得られます.

![](/images/my_histogram.png)

## さいごに

たまに使うんだけど毎回やり方を忘れてしまうようなことは, しっかりメモしておくべきですね.
