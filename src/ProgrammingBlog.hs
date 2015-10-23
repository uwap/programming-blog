--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module ProgrammingBlog where

import Data.Monoid
import Hakyll
import BlogConfiguration

programmingBlog :: BlogConfiguration
programmingBlog = BlogConfiguration {..}
  where
--------------------------------------------------------------------------------
    feedConfiguration :: FeedConfiguration
    feedConfiguration = FeedConfiguration
      { feedTitle       = "λu.wap Programming Blog: Latest Posts"
      , feedDescription = "A Blog about Programming and Computer Sciences"
      , feedAuthorName  = "uwap"
      , feedAuthorEmail = "me@uwap.name"
      , feedRoot        = siteUrl
      }

    siteUrl :: String
    siteUrl = "http://programming.uwap.name"

    postsFolder :: FilePath
    postsFolder = "posts/programming/"
--------------------------------------------------------------------------------
