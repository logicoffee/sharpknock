--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Hakyll
import           Hakyll.Web.Sass (sassCompiler)
import           Skylighting (styleToCss, kate)
import           Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["css/highlight.css"] $ do
        route idRoute
        compile $ makeItem $ compressCss $ styleToCss kate

    match "css/*.scss" $ do
        route   $ setExtension "css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/**" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions wOptions
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
    
    archivePaginate <- buildPaginateWith
        (sortRecentFirst >=> return . paginateEvery 10)
        "posts/**"
        (fromCapture "archive/*.html" . show)
    
    paginateRules archivePaginate $ \number pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = 
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    paginateContext archivePaginate number   <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    teaserField "teaser" "content" <>
    dateField "date" "%B %e, %Y"   <>
    defaultContext

wOptions :: WriterOptions
wOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax ""}


