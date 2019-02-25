module Lib.Context where

import           Hakyll.Core.Compiler          (loadSnapshot)
import           Hakyll.Core.Compiler.Internal (Snapshot)
import           Hakyll.Core.Item              (Item (itemBody, itemIdentifier))
import           Hakyll.Core.Util.String       (needlePrefix)
import           Hakyll.Web.Template.Context   (Context, field)

descriptionField :: String -> Snapshot -> Context String
descriptionField key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    case needlePrefix descriptionSeparater body of
        Nothing -> fail $
            "Hakyll.Web.Template.Context: no teaser defined for " ++
            show (itemIdentifier item)
        Just t -> return $ rawContent t

descriptionSeparater = "<!--more-->" :: String

rawContent :: String -> String
rawContent = tail' . takeWhile (/= '<') . dropWhile (/= '>') where
    tail' [] = []
    tail' xs = tail xs
