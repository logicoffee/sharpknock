{-# LANGUAGE OverloadedStrings #-}

module Trans.Highlight where

import qualified Data.Text              as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk

modifyClass :: Pandoc -> Pandoc
modifyClass source = walk modifyClass' source

modifyClass' :: Block -> Block
modifyClass' (CodeBlock attr content) = CodeBlock (addPrefix attr) content
modifyClass' b                        = b

addPrefix :: Attr -> Attr
addPrefix (identifier, classes, kvs) = (identifier, modifiedClasses, kvs) where
    modifiedClasses = flip map classes $ \c ->
        if elem c languages
           then T.append "language-" c
           else c

languages :: [T.Text]
languages =
    [ "bash"
    , "css"
    , "diff"
    , "docker"
    , "elm"
    , "go"
    , "haskell"
    , "julia"
    , "latex"
    , "lua"
    , "markdown"
    , "none"
    , "purescript"
    , "python"
    , "r"
    , "react"
    , "ruby"
    , "rust"
    , "sql"
    , "stan"
    , "toml"
    , "typescript"
    , "vim"
    , "yaml"
    ]
