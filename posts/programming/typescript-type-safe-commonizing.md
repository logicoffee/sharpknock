---
title: 【Typescript】型を利用して安全に共通化しよう！
published: 2020-05-23
tags: TypeScript
toc: on
---

共通化, してますか？

<!--more-->

はじめは, TypeScript の型システムは貧弱なのでこういうことはできません！て記事を書くつもりだったのですが, 試行錯誤してたらできちゃいました.

てなわけでクイズ形式で試行錯誤の成果を見ていただこうと思います.

## 第一問

### 問題

早速問題です. 2つの関数 `numberAction` と `stringAction` を共通化してください.

```typescript
type NumberAction = Record<'value', number>
type StringAction = Record<'value', string>

const numberAPI = (): number => { 略 }
const stringAPI = (): string => { 略 }

type Dispatch<T> = (t: T) => void

const numberAction = (dispatch: Dispatch<NumberAction>) => {
  const value = numberAPI()
  dispatch({ value })
}

const stringAction = (dispatch: Dispatch<StringAction>) => {
  const value = stringAPI()
  dispatch({ value })
}
```

### 解答例

共通化したいなーって試行錯誤してたのが Redux だったので, Redux 風のコード例を用意しました.

答えは以下のようになるでしょうか. `api` を外から渡すようにすれば型パラメータを利用して共通化することができます.

```typescript
const genericAction = <T>(
  api: () => T
) => (
  dispatch: Dispatch<Record<'value', T>>
) => {
  dispatch({ value: api() })
}
```

## 第二問

### 問題

2つの関数 `numberAction` と `stringAction` を共通化してください.

```typescript
interface Action<T extends string, P> {
  type: T
  value: P
}

type NumberAction = Action<'NUMBER', number>
type StringAction = Action<'STRING', string>

type Dispatch<T> = (t: T) => void

const numberAction = (dispatch: Dispatch<NumberAction>) => {
  const value = numberAPI()
  dispatch({
    type: 'NUMBER',
    value
  })
}

const stringAction = (dispatch: Dispatch<StringAction>) => {
  const value = stringAPI()
  dispatch({
    type: 'STRING',
    value
  })
}
```

### 解答例1

さっきよりも少し複雑になりました. `Action` のフィールドは2つに増えましたが, `api` の戻り値は `value` だけです.

第一問の応用としてこんなふうに共通化が実現できます.

```typescript
const genericAction = <T extends string, P>(
  actionType: T,
  api: () => P
) => (
  dispatch: Dispatch<Action<T, P>>
) => {
  dispatch({
    type: actionType,
    value: api()
  })
}
```

でもこれはちょっと微妙なところがありまして. なにかというと, `T` と `P` の組み合わせが制限できていないところなんですよね.

例えば, `genericAction<'NUMBER', string>` のようなちぐはぐな組み合わせも可能です.

できれば `genericAction<NumberAction>` のように使いたいです.

### 解答例2

てことで少し進化させたのが次の解答例です. これで `genericAction<NumberAction>` のような使い勝手を実現することができました.

```typescript
const genericAction = <A extends Action<string, any>>(
  actionType: A['type'],
  api: () => A['value']
) => (
  dispatch: Dispatch<Action<A['type'], A['value']>>
) => {
  dispatch({
    type: actionType,
    value: api()
  })
}
```

### 解答例2の惜しいところ

解答例2のコードなんですが, 気分としては `dispatch` の型は `Dispatch<A>` と書きたくなります. しかしそうしてしまうと以下のような型エラーが発生します.

```
typescript:Error:2345:Argument of type '{ type: A["type"]; value: A["value"]; }' is not assignable to parameter of type 'A'. '{ type: A["type"]; value: A["value"]; }' is assignable to the constraint of type 'A', but 'A' could be instantiated with a different subtype of constraint 'Action<string, any>'.
```

これは `extends` による型制約が原因です.

`A extends Action<string, any>` ということは, 以下のようなものも型パラメータ `A` に当てはめることができてしまいます.

```typescript
interface A {
  type: string
  value: number
  anotherField: any[]
}
```

この場合 `dispatch` に渡しているオブジェクトは `anotherField` が欠けてしまっているため, エラーが発生してしまうのです.

## ぼやき

普段 Haskell のような表現力豊かな型システムを備えた言語に触れていると, TypeScript が物足りなくなるときがあります.

それでも, TypeScript でもがんばればそれなりに抽象化して共通化することが可能です.

今現在では `extends` による型制約しかないですが, それ以外の型制約についても議論がされているみたいです.

たとえば「ちょうどぴったりの型制約」についてはこの issue で,

[https://github.com/microsoft/TypeScript/issues/35899](https://github.com/microsoft/TypeScript/issues/35899)

「`extends` とは反対の狭める型制約」についてはこの issue で議論されています.

[https://githubcom/microsoft/TypeScript/issues/14520](https://githubcom/microsoft/TypeScript/issues/14520)
