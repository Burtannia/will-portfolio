{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Handler.Modal
import Handler.Project
import Handler.Image

theTitle :: Html
theTitle = toHtml
    ("Will Burton | Gamer & Hobbyist Map Designer" :: Text)

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuth
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        npWidget = getNewProject

    mprofile <- fmap (fmap entityVal) getProfile
    pForm <- genFormIdentify "profile" $ profileForm mprofile

    let editProfile = mkModal "Edit Profile" pForm

    projects <- getAllProjects

    defaultLayout $ do
        setTitle theTitle
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    muser <- maybeAuth
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        npWidget = postNewProject

    mprofile' <- getProfile
    ((pResult, pWidget), pEnctype) <- runFormIdentify "profile" $
        profileForm $ fmap entityVal mprofile'
    
    let pForm = (pWidget, pEnctype)
        editProfile = mkModal "Edit Profile" pForm
    
    mprofile <- do
        let defVal = fmap entityVal mprofile'
        case pResult of
            FormSuccess profile -> do
                let dbF = maybe
                        (void . insert)
                        (replace . entityKey)
                        mprofile'
                runDB $ dbF profile

                for_ mprofile' $ \ep -> do
                    let oldAv = profileAvatar $ entityVal ep
                        newAv = profileAvatar profile
                    when (oldAv /= newAv) $ deleteImage oldAv

                return $ Just profile

            FormMissing ->
                return defVal

            FormFailure errs -> do
                liftIO $ putStrLn "postHomeR Edit Profile"
                liftIO $ print errs
                return defVal

    projects <- getAllProjects
    
    defaultLayout $ do
        setTitle theTitle
        $(widgetFile "homepage")

getProfile :: Handler (Maybe (Entity Profile))
getProfile = fmap listToMaybe $
    runDB $ selectList [] [Asc ProfileName]

getAllProjects :: Handler [Entity Project]
getAllProjects = runDB $
    selectList [ProjectPublished ==. True] [Asc ProjectTitle]

profileForm :: Maybe Profile -> Form Profile
profileForm mp extra = do
    let defs = withClass "form-control" ""
        isOptional = Just "Optional"
        avatarTip = "An image with a 1:1 aspect ratio which is 300px in width and height is recommended."

    (nameRes, nameView) <- mreq textField defs (profileName <$> mp)
    (bioRes', bioView) <- mreq textareaField defs (Textarea . profileBio <$> mp)
    (avRes, avView) <- mreq imageField (withTooltip avatarTip imageSettings) (profileAvatar <$> mp)
    (fbRes, fbView) <- mopt urlField defs (profileFacebook <$> mp)
    (igRes, igView) <- mopt urlField defs (profileInstagram <$> mp)
    (twitchRes, twitchView) <- mopt urlField defs (profileTwitch <$> mp)
    (twitterRes, twitterView) <- mopt urlField defs (profileTwitter <$> mp)
    (ytRes, ytView) <- mopt urlField defs (profileYoutube <$> mp)

    let bioRes = fmap unTextarea bioRes'
        res = Profile <$> nameRes <*> bioRes <*> avRes <*> fbRes <*> igRes <*> twitchRes <*> twitterRes <*> ytRes
        wgt =
            [whamlet|
                #{extra}
                ^{mkWidgetBs4 nameView "Name" Nothing}
                ^{mkWidgetBs4 bioView "Bio" Nothing}
                ^{mkWidgetBs4 avView "Avatar" Nothing}
                ^{mkWidgetBs4 fbView "Facebook" isOptional}
                ^{mkWidgetBs4 igView "Instagram" isOptional}
                ^{mkWidgetBs4 twitchView "Twitch" isOptional}
                ^{mkWidgetBs4 twitterView "Twitter" isOptional}
                ^{mkWidgetBs4 ytView "YouTube" isOptional}
            |]
    
    return (res, wgt)