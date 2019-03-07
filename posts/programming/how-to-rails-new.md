---
title: Rails newしたときの個人的メモ
published: 2019-03-07
tags: Rails
---

タイトル通りです.

<!--more-->

個人ブログのいいところは些細なメモも残しておける点ですね. Git管理もできるし.

```
# rubyのバージョン確認

$ rbenv local
```

```
$ bundle init
$ nvim Gemfile
```

```ruby
# Gemfileの編集. コメントインするだけ.

gem 'rails', '~> 5.2.2'
```

```
$ bundle install --path vendor/bundle --jobs=4

$ git add Gemfile Gemfile.lock
$ git commit -m "Install rails"
```

```
$ bundle exec rails new . -d postgresql -B --skip-turbolinks
```

```
# railsが作った.gitignoreを編集

$ echo "/vendor" >> .gitignore
```

`rails new`時の`-B`オプションによって, まだ`bundle install`されていない. その前にGemfileを編集する.

- 一番下の行を消す(Windows用だから)
- `sass-rails'を'sassc 2.0.1'に変更
- 'sorcery 0.13.0'を追加
- 'haml-rails 2.0.0'を追加

使いたいGemを指定できたら`bundle install`する.

これでおわり.
