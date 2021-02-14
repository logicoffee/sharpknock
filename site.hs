{-# LANGUAGE OverloadedStrings #-}
import           Compilers
import           Control.Monad
import           Hakyll
import           Hakyll.Web.Sass (sassCompiler)
import           Lib.Context     (descriptionField)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    tags       <- buildTags       "posts/**" (fromCapture "tags/*/1.html")
    categories <- buildCategories "posts/**" (fromCapture "categories/*/1.html")
    let postCtx = postContextWith categories tags

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/highlight/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*.scss" $ do
        route   $ setExtension "css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

    match (fromList ["about.md", "contact.md", "categories.md", "privacy-policy.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/**" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWithToc
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let indexCtx =
                    listField "posts" postCtx (return $ take 10 posts) <>
                    constField "title" "Home"                         <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    --------- Categories ------------------------------------------------------
    tagsRules categories $ \category patt -> do
        categoryPaginate <- buildPaginateWith
            (sortRecentFirst >=> return . paginateEvery 10)
            patt
            (fromCapture (fromGlob ("categories/" ++ category ++ "/*.html")) . show)
        paginateRules categoryPaginate $ \pageNum ptn -> do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll ptn
                let ctx =
                        listField "posts" postCtx (return posts) <>
                        paginateContext categoryPaginate pageNum <>
                        constField "title" ("カテゴリー" ++ category) <>
                        defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

    --------- Tags ------------------------------------------------------------
    tagsRules tags $ \tagName patt -> do
        tagsPaginate <- buildPaginateWith
            (sortRecentFirst >=> return . paginateEvery 10)
            patt
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
        (sortRecentFirst >=> return . paginateEvery 15)
        "posts/**"
        (fromCapture "archives/*.html" . show)

    paginateRules archivePaginate $ \pageNum patt -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll patt
            let ctx =
                    listField "posts" postCtx (return posts) <>
                    paginateContext archivePaginate pageNum  <>
                    constField "title" "Archives"            <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let ctx =
                    constField "root" root                   <>
                    listField "posts" postCtx (return posts) <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" ctx

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postContextWith :: Tags -> Tags -> Context String
postContextWith categories tags =
    constField "root" root
    <> categoryField "category" categories
    <> tagsField "tags" tags
    <> descriptionField "description" "content"
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

root :: String
root = "https://sharpknock.com"
