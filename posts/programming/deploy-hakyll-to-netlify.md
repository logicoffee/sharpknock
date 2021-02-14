---
title: HakyllブログをCircleCIつかってNetlifyにデプロイするための設定
published: 2019-01-25
tags: Hakyll, Netlify, CircleCI, Docker
toc: on
---

静的サイトジェネレーターであるHakyllを使ってブログを構築しました. NetlifyにはCD(継続的デプロイ)の機能がありますが, Haskellには標準対応してないみたいです. ならば外部のCIサービスを利用するしかないわけですが, CircleCIを採用しました. 本記事ではHakyllブログをNetlifyにデプロイするためのCircleCIの設定を解説します.

<!--more-->

{toc}

本記事は[こちらの記事](https://nazarii.bardiuk.com/posts/hakyll-circle.html)を参考にさせてもらいました. この記事さまさまです.

ではやっていきましょう.

## 前提
lts-12.26の環境でHakyllブログを構築していることを前提とします. lts-13台にはhakyllが含まれていないため, lts-12台の暫定最新版であるlts-12.26を使っていくことにします. CircleCIで使うDockerイメージもこのスナップショットに合わせて作ってあります.

また, Netlifyには既にHakyllリポジトリに紐付けられたサイトが登録されているものとします. サイト固有のAPI IDを後で使うので, ここまでは終わらせておきましょう.

あとは当然のことですが, CircleCIへの登録も済ませておきましょう.

## CircleCIにリポジトリを登録
CircleCIにログインしたあとのページの左のカラムから, 「ADD PROJECTS」をクリックします. 自分のリポジトリが一覧になっているので, Hakyllブログのリポジトリを「Set Up Project」しましょう.

下のほうにある「Start building」をクリックします. するとビルドは失敗します. 当たり前ですね. なぜなら設定ファイルを追加していないからです. 

どうしてわざわざビルドを失敗させたのかというと, プロジェクトの設定画面へ行くためです. 一度でもビルドを試みたことがあるリポジトリしか, 個別の設定画面へは行けない仕様になっているようです. ここは改善してほしいところですね...

## 環境変数の設定
左のカラムから「JOBS」を選択します. さきほどビルドを失敗させたおかげで, Hakyllブログのリポジトリ名が表示されているはずです. その隣にある歯車マークをクリックしましょう.

「Environment variables」へ進みます. 「Add Variable」から, 新たな環境変数を追加することができます. 

設定すべき環境変数は以下の2つです.

- NETLIFY_AUTH_TOKEN
- NETLIFY_SITE_ID

前者はNetlifyのチーム(私の場合はGitHubに登録した本名になっています)に紐付いたもので, 後者はサイトに紐付いたものです.

`NETLIFY_AUTH_TOKEN`はOAuth applicationsから追加できます[(詳しくはこちら)](https://www.netlify.com/docs/cli/#obtain-a-token-in-the-netlify-ui).

`NETLIFY_SITE_ID`はSite settingsのAPI IDです[(詳しくはこちら)](https://www.netlify.com/docs/cli/#link-with-an-environment-variable).

これら2つをCircleCIの環境変数へと追加しましょう.


## CircleCI用の設定ファイル
あとやることは設定ファイルを追加するだけです.

Hakyllブログのプロジェクトルートに`.circleci`というディレクトリを作成し,　その中に設定ファイルを作成します. 私の設定ファイルをほとんどそのまま利用できるので, プロジェクトルートで以下を実行してもらうのが早いと思います.

```none
$ curl -o .circleci/config.yml --create-dirs https://raw.githubusercontent.com/logicoffee/sharpknock/master/.circleci/config.yml
```

こんな設定ファイルが作成されるかと思います.

```yaml
version: 2
jobs:
  build:
    docker:
      - image: logicoffee/hakyll_netlify:lts-12.16
    steps:
      - checkout
      - restore_cache:
          name: Restore Cached Dependencies
          keys:
            - stack-{{ .Branch }}-{{ checksum  "stack.yaml" }}
            - stack-{{ .Branch }}-
            - stack-
      - run:
          name: Resolve Dependencies
          command: stack build --dependencies-only
      - save_cache:
          name: Cache Dependencies
          key: stack-{{ .Branch }}-{{ checksum "stack.yaml" }}
          paths:
            - ~/.stack
            - ./.stack-work
      - run:
          name: Build Site App
          command: stack build --pedantic
      - run:
          name: Generate Static Site
          command: stack exec [command name] build ### ここを修正！
      - run:
          name: Publish to Netlify
          command: netlify deploy --dir=_site --prod
```
修正すべき点は下のほうにあるサイトのビルドコマンド名です. 上の`curl`コマンドでコピーした場合, ビルドコマンド名はこのブログの名前である`sharpknock`になっています. これをcabalファイルの`excutable`の欄に書かれているものに修正しておいてください.

設定ファイルが用意できたら`git commit`して`git push`しましょう. すると自動的にビルドが開始されるはずです. 最初は多少時間がかかるかもしれません. これでデプロイが済んでるはずですがどうでしょうか？ うまくいかなかった場合はエラーメッセージを見て個別に対応していくしかないでしょう.


## config.yml解説
最後に上のファイルにはなにが書かれているのかを解説しておきます. 設定を修正する際に参考にしてください.

もっと詳しく知りたい場合は[公式ドキュメント](https://circleci.com/docs/2.0/configuration-reference)を参照してください.

### version
CircleCIのバージョンです.

### jobs
ビルドやデプロイなどの一連の流れを`job`と呼んでいます.

### build
これは`job`の名前です. 自分で好きに決められます. もし複数の`job`を定義する場合は重複してはなりません.

### docker
CircleCIには`executer`という概念が存在します. 簡単に言うと「各ステップを行う場所」のことです. `executer`は3種類あります.

- docker
- machine
- macos

2つめの`machine`というのはLinuxのVirtual Machineのことみたいですね. 詳しくは[こちら](https://circleci.com/docs/2.0/executor-types/)を参照してください.

ここではDockerを使いたいので, Dockerイメージとともに`docker`を指定しています. 指定されたDockerイメージは私が作ったものです.

### steps
ひとつの`job`をいくつかのステップに分けることができます. たくさんのコマンドで1つのステップにすることもできますし, 1つのコマンドで1つのステップにすることもできます. 各ステップには名前をつけることができるので, 自分が分かりやすいようにしておけば良いでしょう.

#### checkout
これは特別に用意されたステップです. `git checkout`と同じ感じですね. Working directoryがプロジェクトルートに移動するだけです.

#### restore_cache, save_cache
CircleCIではビルドやデプロイの時間を短縮するためにキャッシュを使っています. `save_cache`までのステップがキャッシュされ, 次回のビルドに利用されます. `restore_cache`のタイミングで, 前回のビルドのキャッシュを利用します.

#### run
各コマンドを実行しています.


## さいごに
もっといろいろ最適化できそうな感じがします. とくにビルドキャッシュのところ. Dockerイメージも普通のHaskellのやつでもいいのかなあ...？ Haskellの難しさのひとつが情報の少なさですね.

