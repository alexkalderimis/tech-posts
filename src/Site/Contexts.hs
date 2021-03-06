{-# LANGUAGE OverloadedStrings #-}

module Site.Contexts where

import           Hakyll

import           Control.Applicative
import           Data.Char               (toLower)
import           Data.Digest.Pure.MD5    (md5)
import           Data.Function           (on)
import           Data.List               (groupBy)
import           System.Process          (readProcess)

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Locale.Compat (defaultTimeLocale)

repo :: String
repo = "https://gitlab.com/alexkalderimis/tech-posts"

baseContext :: Context String
baseContext = defaultContext <> avatarContext

avatarContext :: Context String
avatarContext = constField "gravatarHash" (show $ md5 "alex.kalderimis@gmail.com")

archiveCtx :: Pattern -> Context String
archiveCtx pat = mconcat
  [ yearlyArchives pat
  , constField "title" "Archives"
  , baseContext
  ]

postCtx :: Context String
postCtx = mconcat
  [ dateField "date" "%B %e, %Y"
  , dateField "dateArchive" "%b %e"
  , gitTag "git"
  , postTags "postTags"
  , draftField "isDraft"
  , debugMetaField
  , baseContext
  ]

debugMetaField :: Context a
debugMetaField = field "meta" $ \item -> do
  let ident = itemIdentifier item
  meta <- getMetadata ident
  pure (show meta)

draftField :: String -> Context a
draftField key = field key $ \item -> do
  let ident = itemIdentifier item
  draft <- getMetadataField ident "draft"
  case draft of
    Just "true" -> pure "true"
    _           -> empty

gitTag :: String -> Context String
gitTag key = field key $ \_ -> unsafeCompiler $ do
    sha     <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%H"] []
    message <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%s"] []
    return $ mconcat ["<a href=\"", repo, "/-/commit/", sha
                     , "\" title=\"", message, "\">"
                     , take 8 sha, "</a>"
                     ]

customTitleField :: String -> Context String
customTitleField = constField "pageTitle"

postTags :: String -> Context a
postTags k = listField k
  (field "tag" tagc <> field "href" (fmap slugify . tagc))
  (getUnderlying >>= getTags >>= mapM makeItem)
  where
    tagc = pure . itemBody

slugify :: String -> String
slugify = fmap (toLower . despace)
  where
    despace ' ' = '-'
    despace c   = c

groupedArchives :: Pattern -> Compiler [Item (Integer, [Item String])]
groupedArchives pat =
  mapM makeItem =<<
    map combineItems . groupBy ((==) `on` fst)
      <$> (mapM addYear =<< recentFirst =<< loadAll (pat .&&. hasNoVersion))
  where
    combineItems :: [(Integer, Item String)] -> (Integer, [Item String])
    combineItems = foldr (\(year, item) (_, items) -> (year, item : items)) (0, [])

    addYear :: Item String -> Compiler (Integer, Item String)
    addYear item = do
      year <- yearFromUTC <$> (getItemUTC defaultTimeLocale . itemIdentifier $ item)
      return (year, item)

    yearFromUTC :: UTCTime -> Integer
    yearFromUTC utcTime =
      let (year, _, _) = toGregorian $ utctDay utcTime
      in year

yearlyArchives :: Pattern -> Context a
yearlyArchives pat =
  listField "archives" (year <> items) (groupedArchives pat)
  where
    year = field "year" (return . show . fst . itemBody)
    items = Context $ \k _ i ->
              if k == "items"
                then return $ ListField postCtx (snd . itemBody $ i)
                else empty
