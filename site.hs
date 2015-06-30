--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Hakyll hiding (getTags)
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
--------------------------------------------------------------------------------
activeMenuButton :: String -> Context a
activeMenuButton = flip constField "true" . ("nav"++)
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
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
        compile $ defaultPandocCompiler "about"
    match "contact.markdown" $ do
        route   $ setExtension "html"
        compile $ defaultPandocCompiler "contact"

    match "posts/*" $ do
        route $ setExtension "html"
        compile postCompiler

    create ["archive.html"] $ do
        route idRoute
        compile archiveCompiler

    create ["feed.rss"] $ do
        route idRoute
        compile feedCompiler

    match "index.markdown" $ do
        route $ setExtension "html"
        compile indexCompiler

    create ["tags"] $ do
        route $ setExtension "html"
        let ctx = activeMenuButton "tags" <> constField "title" "Tags" <> globalContext
        compile $ makeItem "" >>= loadAndApplyTemplate "templates/tagcloud.html" ctx
                              >>= loadAndApplyTemplate "templates/default.html" ctx
                              >>= relativizeUrls

    create ["tags/*"] $ do
        tags <- getTags "posts/*"
        tagsRules tags $ \tag pattern -> do
          route idRoute
          compile $ tagCompiler tag pattern

    match "templates/*" $ compile templateCompiler
--------------------------------------------------------------------------------
globalContextWithoutPosts :: Context String
globalContextWithoutPosts = field "tagcloud" (const $ renderTagCloud 100 3000 =<< getTags "posts/*")
                         <> defaultContext

globalContext :: Context String
globalContext = listField "posts_recent10" postCtx (getPosts (Just "content") (Just 10))
             <> listField "posts_all" postCtx (getPosts (Just "content") Nothing)
             <> globalContextWithoutPosts

postCtx :: Context String
postCtx = field "tags" ((renderTagCloud 100 100 =<<) . getTags . fromList . return . itemIdentifier)
        <> dateField "date" "%e. %B %Y"
        <> dateField "rawdate" "%Y-%d-%m"
        <> teaserField "teaser" "content"
        <> constField "siteurl" siteUrl
        <> activeMenuButton "posts"
        <> globalContext

feedCtx :: Context String
feedCtx = postCtx <> bodyField "description"

archiveCtx :: Context String
archiveCtx = constField "title" "Archive"
          <> activeMenuButton "posts"
          <> globalContext

indexCtx :: [Item String] -> Context String
indexCtx posts = listField "posts_left" postCtx (postsl posts)
              <> listField "posts_right" postCtx (postsr posts)
              <> constField "title" "Home"
              <> constField "postblock" "true"
              <> activeMenuButton "home"
              <> globalContext
            where
              postsl = return . fmap snd . filter (((0 :: Int) /=) . (`mod` 2) . fst) . zip [1..]
              postsr = return . fmap snd . filter (((0 :: Int) ==) . (`mod` 2) . fst) . zip [1..]

tagCtx :: String -> Context String
tagCtx tag = constField "title" tag
          <> globalContext
--------------------------------------------------------------------------------
getPosts :: Maybe String -> Maybe Int -> Compiler [Item String]
getPosts Nothing         Nothing    = recentFirst =<< loadAll "posts/*"
getPosts (Just snapshot) Nothing    = recentFirst =<< loadAllSnapshots "posts/*" snapshot
getPosts snapshot        (Just num) = fmap (take num) . recentFirst =<< getPosts snapshot Nothing

getTags :: MonadMetadata m => Pattern -> m Tags
getTags pattern = buildTags pattern (fromCapture "tags/*.html")
--------------------------------------------------------------------------------
defaultPandocCompiler :: String -> Compiler (Item String)
defaultPandocCompiler str = let ctx = activeMenuButton str <> globalContext in
  pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls

postCompiler :: Compiler (Item String)
postCompiler = pandocCompiler
      >>= saveSnapshot "content" 
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

archiveCompiler :: Compiler (Item String)
archiveCompiler =
  makeItem ""
    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    >>= relativizeUrls

feedCompiler :: Compiler (Item String)
feedCompiler = do
  posts <- getPosts (Just "content") (Just 10)
  renderRss feedConfiguration feedCtx posts

indexCompiler :: Compiler (Item String)
indexCompiler = do
  posts <- getPosts (Just "content") (Just 10)
  pandocCompiler
    >>= applyAsTemplate (indexCtx posts)
    >>= loadAndApplyTemplate "templates/default.html" (indexCtx posts)
    >>= relativizeUrls

tagCompiler :: String -> Pattern -> Compiler (Item String)
tagCompiler tag pattern = makeItem ""
    >>= loadAndApplyTemplate "templates/tag.html" (tagCtx tag)
    >>= loadAndApplyTemplate "templates/default.html" (tagCtx tag)
    >>= relativizeUrls
