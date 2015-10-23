module BlogConfiguration where

import Hakyll

data BlogConfiguration =
  BlogConfiguration { feedConfiguration :: FeedConfiguration
                    , siteUrl           :: String
                    , postsFolder       :: FilePath
                    }
