---
title: Haskellにおけるミュータブル変数としてのモナド
published: 2019-04-20
tags: Haskell
toc: on
---

Haskellでは全ての変数がイミュータブルです. つまり再代入が禁じられています. それでもモナドの力を借りればまるでミュータブル変数を扱っているかのようなプログラムを書くことができます.

<!--more-->

本記事ではミュータブル変数として扱える3つの型`IORef a`, `STRef a`, `StateVar a`を紹介します.

{toc}

## IORef
### 定義
データ型`IORef a`は`base`パッケージの`Data.IORef`で定義されています.

代表的な関数は以下の通りです.

```haskell
newIORef :: a -> IO (IORef a)
readIORef :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()
modifyIORef :: IORef a -> (a -> a) -> IO ()
```

### 使い方
関数の型を見れば使い方はわかりますが一応...

例としてフィボナッチ数を求めるプログラムを書いてみました.

注意しなければならないのは, 全ての関数の戻り値はIOアクションであるという点です.

```haskell
import           Control.Monad (forM_)
import           Data.IORef

main :: IO ()
main = do
    ref1 <- newIORef (1 :: Int)
    ref2 <- newIORef (1 :: Int)
    forM_ [1..10] $ \_ -> do
        x <- readIORef ref1
        y <- readIORef ref2
        writeIORef ref1 y
        writeIORef ref2 (x + y)
    print =<< readIORef ref2
```

実行結果は`144`です.

## STRef
### 定義
データ型`STRef s a`は`base`パッケージの`Data.STRef`で定義されていて, `ST s`モナドの中で使うことができます.

代表的な関数は以下の通りです.

```haskell
newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
```

全て`ST s`モナドアクションが返っているのがわかります. Haskell入門にも書かれていますが, 型変数`s`は常に多相的に扱われるため具体的な型を当てはめることはないみたいです.

### 使い方

```haskell
import           Control.Monad    (forM_)
import           Control.Monad.ST (runST)
import           Data.STRef

main :: IO ()
main = print $ runST $ do
    ref1 <- newSTRef (1 :: Int)
    ref2 <- newSTRef (1 :: Int)
    forM_ [1..10] $ \_ -> do
        x <- readSTRef ref1
        y <- readSTRef ref2
        writeSTRef ref1 y
        writeSTRef ref2 (x + y)
    readSTRef ref2
```

こちらの実行結果も`144`です.

## State

ミュータブル変数を使いたい理由のひとつが, 「状態を扱いたいから」でしょう. 状態といえば`State s`モナドですね.

### 定義
型`State s a `は`transformers`パッケージの`Control.Monad.Trans.State.Lazy`と`Control.Monad.Trans.State.Strict`で定義されています.

代表的な関数は以下の通り.

```haskell
get :: State s s
put :: s -> State s ()
modify :: (s -> s) -> State s ()
gets :: (s -> a) -> State s a
runState :: State s a -> s -> (a, s)
evalState :: State s a -> s -> a
execState :: State s a -> s -> s
```

### 使い方

型引数`s`の部分に整数のタプルを保持することで, フィボナッチ数を求めてみました.

```haskell
import           Control.Monad                  (forM_)
import           Control.Monad.Trans.State.Lazy

fibo :: State (Int, Int) ()
fibo = forM_ [1..10] $ \_ -> modify (\(x, y) -> (y, x + y))

main :: IO ()
main = print $ execState fibo (1, 1)
```

これの実行結果は`(89, 144)`です.

もう少し`State`っぽい例を挙げてみましょう. 

```haskell
import           Control.Monad.Trans.State.Lazy

data Coord = Coord Int Int

moveX :: Int -> State Coord()
moveX a = modify (\(Coord x y) -> Coord (x+a) y)

moveY :: Int -> State Coord()
moveY a = modify (\(Coord x y) -> Coord x (y+a))

howFarFromOrigin :: State Coord Float
howFarFromOrigin = do
    Coord x y <- get
    return $ sqrt (fromIntegral (x*x) + fromIntegral (y*y))

main :: IO ()
main = print $ (`evalState` Coord 0 0) $ do
    moveX 1
    moveY 7
    moveX 2
    moveY (-3)
    howFarFromOrigin
```

実行結果は`5.0`です.


## StateVar
### 定義
型`StateVar a`は`StateVar`パッケージの`Data.StateVar`モジュールで定義されています. ただその前に, このモジュールで定義されている型クラスについて触れておきましょう.

```haskell
class HasGetter t a | t -> a where
    get :: monadIO m => t -> m a

instance HasGetter (IO a) a
instance HasGetter (IORef a) a

class HasSetter t a | t -> a where
    ($=) :: monadIO m => t -> a -> m ()

instance HasSetter (IORef a) a

class HasSetter t a => HasUpdate t a b | t -> a b where
    ($~) :: monadIO m => t -> (a -> b) -> m ()

instance HasUpdate (IORef a) a
```

`StateVar`では読み込み可能か書き込み可能かを分けて定義しています. 各クラスのインスタンスには, 先ほど見た`IORef`も登場していますね.

ちなみに`class HasGetter t a | t -> a`という書き方は, 言語拡張`FunctionalDependencies`によって提供されている機能です. 型`t`に対し型`a`が一意に定まることを表しています.

つまり`HasGetter Int Int`と`HasGetter Int String`という2つのインスタンスは同時に作ることができません.

では`StateVar`の定義です.

```haskell
data StateVar a = StateVar (IO a) (a -> IO())

instance HasGetter (StateVar a) a
instance HasSetter (StateVar a) a
instance HasUpdate (StateVar a) a a
```

getterの役割を持つ`IO a`とsetterの役割を持つ`a -> IO()`から, `StateVar a`型が作られます. なにはともあれ使い方を見てみましょう.

### 使い方
まずは`StateVar`ではなく, 3つの型クラスのインスタンスになっている`IORef`の例から.

```haskell
import           Control.Monad (forM_)
import           Data.IORef
import           Data.StateVar

main :: IO ()
main = do
    ref1 <- newIORef (1 :: Int)
    ref2 <- newIORef (1 :: Int)
    forM_ [1..10] $ \_ -> do
        x <- get ref1
        y <- get ref2
        ref1 $= y
        ref2 $= x + y
    print =<< get ref2
```

次に`StateVar`の例です(結局`IORef`の力を借りていますけどね...).

```haskell
import           Control.Monad (forM_)
import           Data.IORef
import           Data.StateVar

main :: IO ()
main = do
    ref <- newIORef (1 :: Int)
    let sv = StateVar (readIORef ref) (writeIORef ref)

    print =<< get sv
    sv $= 3
    print =<< get sv
    sv $~ (+1)
    print =<< get sv
```
これの実行結果は次の通りです.

```none
1
3
4
```

[wai-session](http://hackage.haskell.org/package/wai-session)というパッケージではWebアプリケーションのセッションを管理するために`StateVar`を使っています([そのソースコードがこちら](https://github.com/singpolyma/wai-session/blob/9249234ee815ddae117c7527c495d5cf35f00135/Network/Wai/Session/Map.hs#L47-L49)).
