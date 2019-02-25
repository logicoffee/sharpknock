{-# LANGUAGE OverloadedStrings #-}

module Toc.Trans where

import           Data.Function                   (on)
import           Data.List                       (groupBy)
import           Data.Tree                       (Forest, Tree (Node))
import           Text.Blaze                      ((!))
import           Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Blaze.Internal             (MarkupM (Empty))
import           Text.Pandoc
import           Text.Pandoc.Walk


headerLevel :: Block -> Int
headerLevel (Header level _ _) = level
headerLevel _                  = error "not header"

filterHeader :: Int -> Block -> [Block]
filterHeader n header@(Header level _ _) =
    [header | level <= n]
filterHeader _ _ = []

makeHeaderForest :: [Block] -> Forest Block
makeHeaderForest = map (\(x:xs) -> Node x (makeHeaderForest xs)) . groupBy ((<) `on` headerLevel)

markupHeader :: Tree Block -> H.Html
markupHeader (Node (Header _ (ident, _, _) inlines) headers)
    | null headers  = H.li link
    | otherwise     = H.li $ link <> H.ul (markupHeaders headers)
        where
            link = H.a ! A.href (H.toValue ("#" ++ ident)) $
                renderPandocToHtml (Pandoc nullMeta [Plain inlines])
markupHeader _ = Empty ()

markupHeaders :: Forest Block -> H.Html
markupHeaders = mconcat . map markupHeader

renderPandocToHtml :: Pandoc -> H.Html
renderPandocToHtml pandoc =
    case runPure (writeHtml5 def pandoc) of
        Left  _    -> Empty ()
        Right item -> item

markupToc :: Forest Block -> H.Html
markupToc headers =
    H.div ! A.id "toc" $ H.ul $ markupHeaders headers

replaceToc :: String -> Block -> Block
replaceToc toc (Para [Str "{toc}"]) = RawBlock "html" toc
replaceToc _ block                  = block

insertToc :: Pandoc -> Pandoc
insertToc source =
    walk (replaceToc toc_str) source where
        headers  = query (filterHeader 3) source
        toc_html = markupToc . makeHeaderForest $ headers
        toc_str  = renderHtml toc_html
