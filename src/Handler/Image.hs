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

imageField :: Field Handler ImageId
imageField = Field
    { fieldParse = \texts files ->
        case files of
            [] ->
                case texts of
                    [] -> return $ Right Nothing
                    imgIdText:_ -> do
                        let trim = pack . topTail . unpack
                        mimg <- runDB $ getBy $ UniqueImageId $ trim imgIdText
                        return $ maybe (Left "Invalid image ID") (Right . Just . entityKey) mimg
            file:_ -> do
                imgId <- uploadImage file
                return $ Right $ Just imgId
    , fieldView = \id' name attrs eval isReq ->
        case eval of
            Left _ ->
                [whamlet|
                    <input id=#{id'} name=#{name} *{attrs} type=file :isReq:required>
                |]
            Right imgId ->
                [whamlet|
                    <div>
                        <input type="hidden" name=#{name} value=#{tshow imgId}>
                        <img src=@{ImagesR $ mkImageUrl imgId}
                            .thumbnail .img-fluid .mb-2 style="max-width: 300px">
                        <input id=#{id'} name=#{name} *{attrs} type=file>
                |]
    , fieldEnctype = Multipart
    }

multiImageField :: Field Handler [ImageId]
multiImageField = Field
    { fieldParse = \_ files ->
        if null files
            then return $ Right Nothing
            else do
                imgs <- mapM uploadImage files
                return $ Right $ Just imgs
    , fieldView = \id' name attrs _ isReq ->
        [whamlet|
            <input ##{id'} name=#{name} *{attrs} type=file :isReq:required multiple>
        |]
    , fieldEnctype = Multipart
    }