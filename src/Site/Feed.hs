{-# LANGUAGE OverloadedStrings #-}

module Site.Feed where

import Hakyll

import Site.Contexts (postCtx)

feedConf :: FeedConfiguration
feedConf = FeedConfiguration
  { feedTitle = "alex.kalderimis.dev"
  , feedDescription = "Personal Site"
  , feedAuthorName = "Alex Kalderimis"
  , feedAuthorEmail = "alex.kalderimis@gmail.com"
  , feedRoot = "http://alex.kalderimis.dev"
  }

createFeed :: Pattern -> Rules ()
createFeed p = create ["atom.xml"] $ do
  route idRoute
  compile $ do
    loadAll (p .&&. hasVersion "feed")
      >>= fmap (take 10) . recentFirst
      >>= renderAtom feedConf (postCtx <> bodyField "description")
