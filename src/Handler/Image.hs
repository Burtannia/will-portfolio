{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Image where

import Import
import System.Directory (removeFile, doesFileExist)
import Data.Time.Format.ISO8601

uploadImage :: FileInfo -> Handler ImageId
uploadImage file = do
    app <- getYesod

    now <- liftIO getCurrentTime

    let mExt = parseExt $ fileContentType file
        dir = appImageDir $ appSettings app
    
    case mExt of
        Nothing -> error "Unsupported file type"
        Just ext -> do
            let uuid = (iso8601Show now) <> "." <> (pack $ toLower $ show ext)
                newImg = Image (pack uuid) ext
            liftIO $ fileMove file $ mkImagePath dir newImg
            imgId <- liftHandler $ runDB $ insert newImg
            return imgId

deleteImage :: ImageId -> Handler ()
deleteImage imgId = do
    mImg <- runDB $ get imgId

    for_ mImg $ \img -> do
        app <- getYesod
        let imgPath = mkImagePath (appImageDir $ appSettings app) img
        liftIO $ removeFile imgPath
        stillExists <- liftIO $ doesFileExist imgPath
        unless stillExists $ runDB $ delete imgId
    
    return ()

parseExt :: Text -> Maybe ImageExt
parseExt "image/jpeg" = Just JPG
parseExt "image/png" = Just PNG
parseExt "image/gif" = Just GIF
parseExt _ = Nothing

mkImagePath :: FilePath -> Image -> FilePath
mkImagePath dir img = dir ++ '/' : (unpack $ imageUuid img)

mkImageUrl :: ImageId -> Route Static
mkImageUrl imgId = StaticRoute [toPathPiece imgId] []