module Toc.Compiler where

import           Hakyll
import           Text.Pandoc.Options
import           Toc.Trans

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
    , writerHighlightStyle = Nothing
    }
