---
title: 【Golang】Exit, panic, Goexitの違い
published: 2019-02-17
tags: golang
toc: on
---
Go言語のプログラムにおいて, 例外的な事態が発生したときに呼ばれる関数は, 代表的なものが3つあります. それは`Exit`と`panic`と`Goexit`です. この記事ではこれらの違いを説明します.

<!--more-->

{toc}

## はじめに

本記事におけるGoのバージョンは`1.11.5`です.

## 関数定義
3つの関数定義を, パッケージとともに表示すると以下のようになります.
```go
func os.Exit(code int)
func builtin.panic(v interface{})
func runtime.Goexit()
```

## 3つの違い
ざっくり言うと, 上のほうが過激で, 下のほうが穏やかです.

### os.Exit
引数の整数は終了コードです. 今回は例外的な事態が発生した場合を考えているので, 終了コードとしては0以外を考えています.

プログラムの中で`os.Exit(0以外)`が呼ばれると, 以下のような挙動をとります.

- プログラムは直ちに終了する.
- `defer`された関数は呼ばれない.

### builtin.panic
引数にはログ出力したい文字列等を指定します. したがってこんな使い方をすることが多いと思います(`builtin`パッケージはインポートする必要はありません).

```go
if err != nil {
    panic(err)
}
```
`panic`の挙動を説明するために, サンプルコードを用意しました.

```go
# main.go

package main
import fmt

func main() {
    fmt.Print("inside main")
    defer fmt.Print("deferred function inside main")

    SomeFunc()
}

func SomeFunc() {
    fmt.Print("inside SomeFun")
    defer fmt.Print("deferred function inside SomeFunc")

    panic("panic!!!")

    defer fmt.Print("this function wouldn't be called")
}
```
`main`が`SomeFunc`を呼んで, `SomeFunc`が`panic`を呼んでいます. これを実行すると以下の出力が得られます(ユーザー名は隠してあります).

```none
inside main
inside SomeFun
deferred function inside SomeFunc
deferred function inside main
panic: panic!!!

goroutine 1 [running]:
main.SomeFunc()
        /Users/***/Repogitories/github.com/logicoffee/play_with_go/main.go:22 +0xd5
main.main()
        /Users/***/Repogitories/github.com/logicoffee/play_with_go/main.go:15 +0xbe
exit status 2
```

これを言葉で説明するとこうなります.

- `SomeFunc`内で`panic`が呼ばれると処理がストップする.
- `SomeFunc`内でそれまでに`defer`された関数が順次処理される.
- `main`へと処理が戻る.
- `main`からすると, 以下が起こったように見える.

```go
func main() {
    fmt.Print("inside main")
    defer fmt.Print("deferred function inside main")

    panic("panic!!!")
}
```

- したがって`main`内の処理はストップし, それまでに`defer`された関数が順次処理される.
- 最終的にはプログラム全体がストップし, `panic`に渡された引数が表示される.


### runtime.Goexit

おおかた`panic`と同じような挙動をとりますが, 一番違うのは他のgoroutineの終了を待つ点です.

`panic`は他のgoroutineの終了を待たずにプログラム全体が終了します.

`runtime.Goexit`は他のgoroutineが終了するのを見届けてから終了します.

## 使い分け
そもそもこれら3つの関数を直接書くことは少なめかもしれません. しかし直接でなくとも裏で呼び出すケースはありえます. 例えば以下の関数がそうです.

```go
// logパッケージ
log.Fatal() //os.Exit(1)が呼ばれる
log.Panic() //panic()が呼ばれる

// testingパッケージ
T.Fatal() //runtime.Goexit()が呼ばれる
```
テスト内で`log.Fatal()`を呼び出すと, 他に控えているテストが実行されずに終了してしまいます.

テストの準備(テストデータの挿入とか)が失敗した場合には`log.Fatal`を使いたくなりますが, それでは他のテストにまで影響が及んでしまいます. このあたりは使い分けが必要でしょう.
