---
title: withStylesをTypeScriptで使う方法
published: 2019-05-29
tags: React, TypeScript
---

Reactコンポーネントをスタイリングする方法のひとつである, MaterialUIのwithStyles. これをTypeScriptで使う方法を紹介します.

<!--more-->

## バージョン

- TypeScript: 2.9.2
- React: 16.2.0


## コード

コードを見ていただくのが早いでしょうから, 完成形をお見せします.

```tsx
import * as React from 'react'
import {withStyles, WithStyles, Theme} from '@material-ui/core/styles'
import Paper from '@material-ui/core/Paper'

// Theme型の値を受け取り, オブジェクトを返す関数.
// ここにスタイルを定義していく.
const styles = (theme: Theme) => ({
    // myStyleForPaper というキーでスタイルを定義
    myStyleForPaper: {
        ...theme.mixins.gutters(),
        paddingTop: theme.spacing.unit * 2,
        paddingBottom: theme.spacing.unit * 3,
    },
})

// コンポーネントの props 用の interface.
// WithStyles を extends することで classes という prop が使える.
interface Props extends WithStyles<typeof styles> {
    otherPropsYouWantToUse: anyType,
}

const Foo: React.FC<Props> = ({
    // classes という prop 経由でスタイルを適用する.
    classes,
    otherPropsYouWantToUse,
}) => {
    return (
        <Paper
            className={classes.myStyleForPaper}
        >
            こんな風にすれば, 上で定義したスタイルが適用される.
        </Paper>
    )
}

export default withStyles(styles)(Foo)
```

## Theme のデフォルト値
上の例の`theme.spacing`のように, 予め用意されている値を使うことができます. どんなものが用意されているかは次のページにまとめられています.

[https://material-ui.com/customization/default-theme/](https://material-ui.com/customization/default-theme/)
