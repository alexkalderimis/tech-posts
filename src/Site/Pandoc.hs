module Site.Pandoc where

import qualified Site.TOC as TOC

import Text.Pandoc

import Hakyll.Web.Pandoc
import Hakyll (Item(..), Compiler, Context, constField, getMetadataField, getUnderlying, getResourceBody)

tocCompiler :: Compiler (Item String, Context a)
tocCompiler = do
  alignment <- readAlignment <$> (flip getMetadataField "toc" =<< getUnderlying)
  doc <- getResourceBody >>= readPandoc
  let toc = TOC.tableOfContents alignment (itemBody doc)
  return (writePandoc doc, constField "toc" toc)
  where
    readAlignment Nothing = TOC.RightAligned
    readAlignment (Just "right") = TOC.RightAligned
    readAlignment (Just "left" ) = TOC.LeftAligned
    readAlignment (Just "off"  ) = TOC.Disabled
    readAlignment _       = TOC.Disabled

pandocFeedCompiler :: Compiler (Item String)
pandocFeedCompiler =
  pandocCompilerWithTransform readerOptions noMathWriter id
  where noMathWriter = writerOptions { writerHTMLMathMethod = PlainMath }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = MathJax ""
  }

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions

