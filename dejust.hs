import qualified Data.List as L
import Data.Monoid
import Data.Char
import Data.Maybe

main = do
  ls <- dejust . fmap (L.dropWhileEnd isSpace) . lines <$> getContents
  putStr (concat ls)

dejust :: [String] -> [String]
dejust = go False False (Endo id)
  where
    z = Endo id :: Endo [String]
    go _ _ acc [] = appEndo acc []
    go literal prevWasText acc (s:strs) =
      let literal' = case s of
                      ('`':'`':'`':_) -> not literal
                      ('-':'-':'-':_) -> not literal
                      _               -> literal
          prevWasText' = not literal && s /= []
          mid = case (literal, prevWasText) of
                  (True, _) -> Endo ("\n":)
                  (_, True) -> if all (`elem` ['-', '=']) s || startsWith '*' s
                                  then Endo ("\n":)
                                  else Endo (" ":)
                  _         -> if null s then Endo id else Endo ("\n\n":)
       in go literal' prevWasText' (acc <> mid <> Endo (s:)) strs
    startsWith c = (Just c ==) . listToMaybe

