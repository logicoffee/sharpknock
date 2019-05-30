---
title: Haskellプロジェクトの始め方
published: 2019-05-30
tags: Haskell
---

個人的メモです.

<!--more-->

1. GitHubで空っぽのリポジトリを作成
1. ローカルにクローン
1. `stack new <project name> --bare` で雛形作成
1. `package.yaml`の修正
    1. リストのインデントを深くする
    1. `excutables`の名前変更
    1. `package.yaml`の`ghc-options`に`-Wall`追加
    1. リポジトリ名とプロジェクト名が異なる場合は`stack.yaml`を編集
1. `Initial commit`
