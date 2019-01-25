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
    tags       <- buildTags       "posts/**" (fromCapture "tags/*/1.html")
    categories <- buildCategories "posts/**" (fromCapture "categories/*/1.html")
    let postCtx = postContextWith categories tags

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

    match (fromList ["about.md", "contact.md", "categories.md"]) $ do
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
                    listField "posts" postCtx (return $ take 5 posts) <>
                    constField "title" "Home"                         <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    --------- Categories ------------------------------------------------------
    tagsRules categories $ \category pattern -> do
        categoryPaginate <- buildPaginateWith
            (sortRecentFirst >=> return . paginateEvery 10)
            pattern
            (fromCapture (fromGlob ("categories/" ++ category ++ "/*.html")) . show)
        paginateRules categoryPaginate $ \pageNum ptn -> do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll ptn
                let ctx =
                        listField "posts" postCtx (return posts) <>
                        paginateContext categoryPaginate pageNum <>
                        constField "title" category              <>
                        defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

    --------- Tags ------------------------------------------------------------
    tagsRules tags $ \tagName pattern -> do
        tagsPaginate <- buildPaginateWith
            (sortRecentFirst >=> return . paginateEvery 10)
            pattern
            (fromCapture (fromGlob ("tags/" ++ tagName ++ "/*.html")) . show)
        paginateRules tagsPaginate $ \pageNum ptn -> do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll ptn
                let ctx =
                        listField "posts" postCtx (return posts) <>
                        paginateContext tagsPaginate pageNum     <>
                        constField "title" tagName               <>
                        defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

    --------- Archive ---------------------------------------------------------
    archivePaginate <- buildPaginateWith
        (sortRecentFirst >=> return . paginateEvery 10)
        "posts/**"
        (fromCapture "archives/*.html" . show)
        
    paginateRules archivePaginate $ \pageNum pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    listField "posts" postCtx (return posts) <>
                    paginateContext archivePaginate pageNum  <>
                    constField "title" "Archives"            <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
   
    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postContextWith :: Tags -> Tags -> Context String
postContextWith categories tags =
    categoryField "category" categories <>
    tagsField "tags" tags               <>
    teaserField "teaser" "content"      <>
    dateField "date" "%B %e, %Y"        <>
    defaultContext

wOptions :: WriterOptions
wOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }
