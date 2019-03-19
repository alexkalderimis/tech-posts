--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Hakyll

import Site.Contexts
import Site.Feed
import Site.Pandoc (pandocFeedCompiler, tocCompiler)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    let postsPattern = "posts/*"

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "scripts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.md", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" baseContext
            >>= relativizeUrls

    match postsPattern $ do
        route $ setExtension "html"
        compile $ tocCompiler
            >>= (\(doc, toc) -> loadAndApplyTemplate "templates/post.html"
                                                    (toc <> postCtx) doc)
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        let ctx = archiveCtx postsPattern
        compile $ makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["404.html"] $ do
      let fields = customTitleField "Not Found" <> baseContext
      route idRoute
      compile $ makeItem ""
          >>= loadAndApplyTemplate "templates/404.html" baseContext
          >>= loadAndApplyTemplate "templates/default.html" fields

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (postsPattern .&&. hasNoVersion)
            let indexCtx = listField "posts" postCtx (return posts)
                         <> baseContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match postsPattern . version "feed" $ compile pandocFeedCompiler

    createFeed postsPattern

