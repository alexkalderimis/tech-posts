module Site.Contexts where

import           Data.Monoid
import           Hakyll
import System.Process (readProcess)

repo :: String
repo = "https://github.com/alexkalderimis/tech-post"

gitTag :: String -> Context String
gitTag key = field key $ \_ -> unsafeCompiler $ do
    sha     <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%H"] []
    message <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%s"] []
    return $ mconcat ["<a href=\"", repo, "/commit/", sha
                     , "\" title=\"", message, "\">"
                     , take 8 sha, "</a>"
                     ]

customTitleField :: String -> Context String
customTitleField = constField "pageTitle"
