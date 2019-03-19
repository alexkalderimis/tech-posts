{-# LANGUAGE OverloadedStrings #-}

-- gratefully inspired by blankdenum: blaenk/blaenk.github.io
module Site.TOC
  ( tableOfContents
  , Alignment(..)
  ) where

import           Text.Pandoc                     hiding (Alignment)
import           Text.Pandoc.Class               (runPure)
import           Text.Pandoc.Walk                (query)

import           Data.Function                   (on)
import           Data.List                       (groupBy)
import           Data.List.NonEmpty              (NonEmpty ((:|)), nonEmpty)
import           Data.Maybe
import           Data.Text                       (pack)
import           Data.Tree                       (Forest, Tree (Node))

import           Text.Blaze.Html                 (preEscapedToHtml, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

data TocHeader = TocHeader Int Attr [Inline]
  deriving (Show, Eq)

data Subheading = Subheading String [(String, String)] [Inline]
  deriving (Show, Eq)

data Alignment
  = Disabled
  | LeftAligned
  | RightAligned
  deriving (Read, Show, Eq)

headerLevel :: TocHeader -> Int
headerLevel (TocHeader level _ _) = level

collectHeaders :: Block -> [TocHeader]
collectHeaders blk = case blk of
  (Header lvl attr@(_, classes, _) content)
    | "notoc" `notElem` classes -> pure $ TocHeader lvl attr content
  _                             -> mempty

groupByHierarchy :: [TocHeader] -> Forest Subheading
groupByHierarchy =
  fmap (\(x :| xs) ->
         let (TocHeader _ (ident, _, kvs) is) = x
         in Node (Subheading ident kvs is) (groupByHierarchy xs))
  . catMaybes
  . fmap nonEmpty
  . groupBy ((<) `on` headerLevel)

markupHeader :: Tree Subheading -> H.Html
markupHeader (Node (Subheading ident keyvals inline) subheadings) =
  H.li $ link <> maybe mempty (H.ol . markupHeaders) (nonEmpty subheadings)
  where
    render x =
      either (pure "err") id . runPure $
      writeHtml5String def (Pandoc nullMeta [Plain x])
    section = maybe (render inline) pack (lookup "toc" keyvals)
    link = H.a ! A.href (H.toValue $ "#" <> ident) $ preEscapedToHtml section

markupHeaders :: Foldable f => f (Tree Subheading) -> H.Html
markupHeaders = foldMap markupHeader

createTable :: Forest Subheading -> H.Html
createTable headers =
  (H.nav ! A.id "toc") $ do
    H.h3 "Contents"
    H.ol $ markupHeaders headers

generateTOC :: [TocHeader] -> Alignment -> String
generateTOC [] _ = mempty
generateTOC headers alignment =
  case alignment of
    Disabled     -> mempty
    RightAligned -> render . (! A.class_ "right-toc") . table $ headers
    LeftAligned  -> render . table $ headers
  where
    render = renderHtml
    table = createTable . groupByHierarchy

tableOfContents :: Alignment -> Pandoc -> String
tableOfContents alignment ast =
  let headers = query collectHeaders ast
  in generateTOC headers alignment
