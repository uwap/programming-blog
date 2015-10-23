--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Monoid
import Hakyll hiding (getTags)
import BlogConfiguration
import ProgrammingBlog
--------------------------------------------------------------------------------
activeMenuButton :: String -> Context a
activeMenuButton = flip constField "true" . ("nav"++)
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ makeBlog programmingBlog

makeBlog :: BlogConfiguration -> Rules ()
makeBlog conf@(BlogConfiguration {..}) = do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler
  match "js/vendor/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "about.markdown" $ do
    route   $ setExtension "html"
    compile $ defaultPandocCompiler conf "about"
  match "contact.markdown" $ do
    route   $ setExtension "html"
    compile $ defaultPandocCompiler conf "contact"

  match "posts/*" $ do
    route   $ setExtension "html"
    compile $ postCompiler conf

  create ["archive.html"] $ do
    route idRoute
    compile $ archiveCompiler conf

  create ["feed.rss"] $ do
    route idRoute
    compile $ feedCompiler conf

  match "index.markdown" $ do
    route   $ setExtension "html"
    compile $ indexCompiler conf

  create ["tags"] $ do
    route $ setExtension "html"
    let ctx = activeMenuButton "tags" <> constField "title" "Tags" <> globalContext conf
    compile $ makeItem "" >>= loadAndApplyTemplate "templates/tagcloud.html" ctx
                          >>= loadAndApplyTemplate "templates/default.html" ctx
                          >>= relativizeUrls

  create ["tags/*"] $ do
    tags <- getTags "posts/*"
    tagsRules tags $ \tag pattern -> do
      route idRoute
      compile $ tagCompiler conf tag pattern

  match "templates/*" $ compile templateCompiler
--------------------------------------------------------------------------------
globalContextWithoutPosts :: Context String
globalContextWithoutPosts = field "tagcloud" (const $ renderTagCloud 100 120 =<< getTags "posts/*")
                         <> defaultContext

globalContext :: BlogConfiguration -> Context String
globalContext conf = listField "posts_recent10" (postCtx conf) (getPosts (Just "content") (Just 10))
                  <> listField "posts_all" (postCtx conf) (getPosts (Just "content") Nothing)
                  <> globalContextWithoutPosts

postCtx :: BlogConfiguration -> Context String
postCtx conf = field "tags" ((renderTagCloud 100 100 =<<) . getTags . fromList . return . itemIdentifier)
            <> dateField "date" "%e. %B %Y"
            <> dateField "rawdate" "%Y-%d-%m"
            <> teaserField "teaser" "content"
            <> constField "siteurl" (siteUrl conf)
            <> activeMenuButton "posts"
            <> globalContext conf

feedCtx :: BlogConfiguration -> Context String
feedCtx conf = postCtx conf <> bodyField "description"

archiveCtx :: BlogConfiguration -> Context String
archiveCtx conf = constField "title" "Archive"
               <> activeMenuButton "posts"
               <> globalContext conf

indexCtx :: BlogConfiguration -> [Item String] -> Context String
indexCtx conf posts = listField "posts_left" (postCtx conf) (postsl posts)
                   <> listField "posts_right" (postCtx conf) (postsr posts)
                   <> constField "title" "Home"
                   <> constField "postblock" "true"
                   <> activeMenuButton "home"
                   <> globalContext conf
                where
                  postsl = return . fmap snd . filter (((0 :: Int) /=) . (`mod` 2) . fst) . zip [1..]
                  postsr = return . fmap snd . filter (((0 :: Int) ==) . (`mod` 2) . fst) . zip [1..]

tagCtx :: BlogConfiguration -> String -> Pattern -> Context String
tagCtx conf tag pattern = constField "title" tag
                       <> listField "posts_recent10" (postCtx conf) (fmap (take 10) . recentFirst =<< loadAll pattern)
                       <> listField "posts_all" (postCtx conf) (recentFirst =<< loadAll pattern)
                       <> globalContextWithoutPosts
--------------------------------------------------------------------------------
getPosts :: Maybe String -> Maybe Int -> Compiler [Item String]
getPosts Nothing         Nothing    = recentFirst =<< loadAll "posts/*"
getPosts (Just snapshot) Nothing    = recentFirst =<< loadAllSnapshots "posts/*" snapshot
getPosts snapshot        (Just num) = fmap (take num) . recentFirst =<< getPosts snapshot Nothing

getTags :: MonadMetadata m => Pattern -> m Tags
getTags pattern = buildTags pattern (fromCapture "tags/*.html")
--------------------------------------------------------------------------------
defaultPandocCompiler :: BlogConfiguration -> String -> Compiler (Item String)
defaultPandocCompiler conf str = let ctx = activeMenuButton str <> globalContext conf in
  pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls

postCompiler :: BlogConfiguration -> Compiler (Item String)
postCompiler conf = pandocCompiler
      >>= saveSnapshot "content" 
      >>= loadAndApplyTemplate "templates/post.html"    (postCtx conf)
      >>= loadAndApplyTemplate "templates/default.html" (postCtx conf)
      >>= relativizeUrls

archiveCompiler :: BlogConfiguration -> Compiler (Item String)
archiveCompiler conf =
  makeItem ""
    >>= loadAndApplyTemplate "templates/archive.html" (archiveCtx conf)
    >>= loadAndApplyTemplate "templates/default.html" (archiveCtx conf)
    >>= relativizeUrls

feedCompiler :: BlogConfiguration -> Compiler (Item String)
feedCompiler conf = do
  posts <- getPosts (Just "content") (Just 10)
  renderRss (feedConfiguration conf) (feedCtx conf) posts

indexCompiler :: BlogConfiguration -> Compiler (Item String)
indexCompiler conf = do
  posts <- getPosts (Just "content") (Just 10)
  pandocCompiler
    >>= applyAsTemplate (indexCtx conf posts)
    >>= loadAndApplyTemplate "templates/default.html" (indexCtx conf posts)
    >>= relativizeUrls

tagCompiler :: BlogConfiguration -> String -> Pattern -> Compiler (Item String)
tagCompiler conf tag pattern = makeItem ""
    >>= loadAndApplyTemplate "templates/tag.html" (tagCtx conf tag pattern)
    >>= loadAndApplyTemplate "templates/default.html" (tagCtx conf tag pattern)
    >>= relativizeUrls
