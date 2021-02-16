{-# LANGUAGE OverloadedStrings #-}

module Compilers where

import           Hakyll
import           Text.Pandoc.Options
import           Trans.Highlight     (modifyClass)
import           Trans.Toc           (insertToc)

pandocCompilerWithToc :: Compiler (Item String)
pandocCompilerWithToc = do
    identifier <- getUnderlying
    enabled <- getMetadataField identifier "toc"
    case enabled of
        Nothing -> pandocCompilerWithTransform defaultHakyllReaderOptions wOptions modifyClass
        Just _  -> pandocCompilerWithTransform defaultHakyllReaderOptions wOptions (insertToc . modifyClass)

wOptions :: WriterOptions
wOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    , writerHighlightStyle = Nothing
    }
