---
title: 【Rails】結合先のテーブルで条件つけたいけど, 結合元のレコードは全部欲しいってときはScoped Association
published: 2019-02-04
tags: Rails
---

本記事の内容をしっかり詰め込んだらタイトルが長くなりました. つまりは「SQLのON句に条件をつけたいけど, RailsなんだからSQLベタ書きはヤダ！」ってときに使えるテクニックをご紹介します.

<!--more-->

## はじめに
本記事は[こちらのページ](https://revs.runtime-revolution.com/conditional-eager-loading-in-rails-9b1c1c592897)を参考にしています.

また, SQLやActiveRecordのクエリメソッドについて大体の理解を仮定して進めていきます(僕自身Railsを使っているのでSQLはそんなに書いたことないですけれど).

以下で登場する問題設定は, [こちら](https://github.com/logicoffee/rails_experiment/tree/scoped-association)に用意してあります. クローンしてくれば手元で実験が行えます. 実験のやり方等はREADMEに書いてあるのでご参照ください.

ではやっていきましょう.

## 問題設定
まずは具体的な状況を設定しておきましょう. 今から設定するのは, 「結合先のテーブルで条件つけたいけど, 結合元のレコードは全部欲しい」というものです. タイトルにも書いているやつですね.

例えばチームモデルとプレーヤーモデルがあり, 関係が1対0以上だったとしましょう. さらにプレーヤーモデルは`deleted`という論理削除用の属性を持っているとします. 例えばこんな感じです.

|チーム   |プレーヤー  |論理削除|
|:-------|:---------|:------|
|チームA  |重田しげる  |false  |
|        |佐藤里子    |false  |
|チームB  |鈴木すず    |false  |
|        |高橋たか子  |false   |
|チームC  |神崎かん太  |true    |

この例ではチームCの神崎かん太が論理削除されている状態です.

このような場合に, 各チームのプレーヤー一覧を表示させたいとします.


<div class="framed">

### チームA

- 重田しげる
- 佐藤里子

### チームB

- 鈴木すず
- 高橋たか子

### チームC

このチームにプレーヤーはいません.

</div>

プレーヤーがいないチームCも一覧に表示されているのがポイントです.

以上の設定が, 「結合先のプレーヤーテーブルにおいて論理削除されていないという条件をつけたいけど, チームのレコードは全部欲しい」という状況になっているのがお分かりいただけるでしょうか.


## どうクエリメソッドを書けばよいか？

### まずパッと思いつくもの

例えばこんなのはどうでしょうかね.

```ruby
Team.eager_load(:players).where(players: {deleted: false})
```

すると以下のようなSQLが発行されます.

```sql
SELECT 
  "teams"."id" AS t0_r0,
  "teams"."name" AS t0_r1,
  "teams"."created_at" AS t0_r2,
  "teams"."updated_at" AS t0_r3,
  "players"."id" AS t1_r0,
  "players"."name" AS t1_r1,
  "players"."deleted" AS t1_r2,
  "players"."team_id" AS t1_r3,
  "players"."created_at" AS t1_r4,
  "players"."updated_at" AS t1_r5 
FROM
  "teams" 
  LEFT OUTER JOIN "players" 
    ON "players"."team_id" = "teams"."id" 
WHERE
  "players"."deleted" = 0
```

この場合だと, チームCは取ってこれません... 実際チーム数を数えてみると2つです.

```ruby
teams = Team.eager_load(:players).where(players: {deleted: false})
teams.size #=> 2
```
(ちなみに`t.count`は追加でSQLを発行してしまうので注意です.)

### このSQLのなにがまずいのか
上のクエリメソッドで発行されるSQLの問題点は, `WHERE`句に論理削除条件が入っていることです. これによって, `LEFT OUTER JOIN`であるにもかかわらず, チームCのレコードが取ってこれませんでした.

ではどうなっていたらよかったかというと, 以下のように論理削除条件は`ON`句にあるべきだったのです.

```sql
SELECT 
  "teams"."id" AS t0_r0,
  -- 中略
  "players"."updated_at" AS t1_r5 
FROM
  "teams" 
  LEFT OUTER JOIN "players" 
    ON "players"."team_id" = "teams"."id" 
      AND "players"."deleted" = 0
```

このSQL文が発行できれば, チームCも逃すことはありません.

### 解決策

ここからが本題ですね. これまで見てきた問題を解決するには, Scoped Associationsというものを使います.

言葉で説明するよりコードを見てもらったほうが早いでしょう.

```ruby
# モデルクラス定義

class Team
  has_many :players, -> { where(deleted: false) }
end
```

```ruby
# クエリメソッド

Team.eager_load(:players)
```
これで, `ON`句に論理削除条件が入ったSQLを発行することができます.

ポイントはなんといっても`Team`モデルクラスの定義ですね. `has_many`メソッドにラムダを渡しています. ここで論理削除条件を指定しています.

これはつまり, **`Team`から`Players`を参照する際は必ず`deleted: false`という条件がつく**ということです.

例えば以下のような使い方をしたときもデフォルトで論理削除条件がつくようになっています.

```ruby
team = Team.first
team.players
```

```sql
-- t.players で発行されるSQL

SELECT
  "players".*
FROM
  "players"
WHERE
  "players"."team_id" = ?
  AND "players"."deleted" = 0
```

### デフォルトで設定されるのは困るなーってときは？
今回の論理削除されているかどうかという条件は, デフォルトになっていてもいいくらいの条件でした. しかし絞り込み条件を毎回使うとは限らない場合はどうしたらよいでしょうか.

それは簡単です. 以下のようにすれば解決できます.

```ruby
class Team
  has_many :players
  has_many :existing_players, -> { where(deleted: false) }, class_name: 'Player`
end
```

こうすれば, `Team.eager_load(:existing_players)`とすることで, チームCも取得できます. ただし注意してほしいのは, この場合`players`ではなく`existing_players`で参照する必要があります.

```ruby
teams = Team.eager_load(:existing_players)
teams.each do |team|
  team.existing_players  # team.players としないように注意
  # その他処理
end
```


## まとめ
「関連テーブルの条件を`ON`句に書きたい！」という問題は, 実際にRailsアプリを構築している際にぶち当たったものです. その時はなかなか検索しても出てこなかったので苦労しました...

冒頭で紹介した参考記事を見つけたときはそれはもう感動しましたね.

今回説明したScoped Associationsは使えるケースが他にもありそうです. どちらにせよコントローラに長々とクエリメソッドを書くのはあまり美しくないですしね. よく使うクエリはモデルにまとめておくのがよさそうです.
