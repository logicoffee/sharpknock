module Src.Compiler where

import           Hakyll
import           Src.Toc
import           Text.Pandoc.Options

pandocCompilerWithToc :: Compiler (Item String)
pandocCompilerWithToc = do
    identifier <- getUnderlying
    enabled <- getMetadataField identifier "toc"
    case enabled of
        Nothing -> pandocCompilerWith defaultHakyllReaderOptions wOptions
        Just _  -> pandocCompilerWithTransform defaultHakyllReaderOptions wOptions insertToc

wOptions :: WriterOptions
wOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }
