{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Component where

import Import
import Handler.Image
import Handler.Modal

getComponent :: Component -> Widget
getComponent (C_ImageGroup is) =
    -- make form
    $(widgetFile "components/image-group")

postComponent :: Component -> Widget
postComponent cId = return ()

newComponent :: Widget
newComponent = return ()

deleteComponent :: Component -> Handler ()
deleteComponent (C_ImageGroup is) = mapM_ deleteImage is
deleteComponent (C_Markup mId) = runDB $ delete mId
deleteComponent _ = return ()

data CreateComp
    = CC_ImageGroup (Maybe [ImageId])
    | CC_VideoEmbed (Maybe Text)
    | CC_Markup (Maybe MarkupBlockId)
    deriving Show

-- compForm :: CreateComp -> Form Component
-- compForm (CC_ImageGroup mis) = do
--     (imgRes, imgView) <- mmulti fileField uploadSettings vals 1 bs4FASettings
    
--     where
--         vals = fromMaybe [] mis
--         uploadSettings = FieldSettings
--             { fsLabel = ""
--             , fsTooltip = Nothing
--             , fsId = Nothing
--             , fsName = Nothing
--             , fsAttrs =
--                 [ ("accept", ".jpg, .png, .gif")
--                 , ("class", "form-control-file") ]
--             }

-- FIXME: imageid fails to fetch image from DB
-- maybe just pass an image id to the function explicitly?
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
                        <img .thumbnail .img-fluid src=@{ImagesR $ mkImageUrl imgId}>
                        <input id=#{id'} name=#{name} *{attrs} type=file>
                |]
    , fieldEnctype = Multipart
    }