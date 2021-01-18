{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Project where

import Import

import Handler.Image
import Handler.Modal

import Text.Julius (RawJS (..))
import qualified Data.Text as T (foldr)
import Data.Maybe (fromJust)

getProjectR :: ProjectId -> Handler Html
getProjectR projectId = do
    muser <- maybeAuth
    project <- runDB $ get404 projectId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = projectPublished project

    when (not isAdmin && not published) notFound

    pForm <- genFormIdentify pFormIdent $ projectForm $ Just project

    let pWidget = mkModal "Edit Project" pForm

    defaultLayout $ do
        setTitle $ toHtml $ projectTitle project
        $(widgetFile "project")

postProjectR :: ProjectId -> Handler Html
postProjectR projectId = do
    muser <- maybeAuth
    project <- runDB $ get404 projectId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = projectPublished project

    when (not isAdmin && not published) notFound

    ((pResult, pWidget'), pEnctype) <- runFormIdentify pFormIdent $ projectForm $ Just project

    let pForm = (pWidget', pEnctype)
        pWidget = mkModal "Edit Project" pForm

    case pResult of
        FormSuccess newProject -> do
            let oldIcon = projectIcon project
                newIcon = projectIcon newProject

            runDB $ replace projectId newProject

            when (oldIcon /= newIcon) $ deleteImage oldIcon

            redirect $ ProjectR projectId

        FormMissing -> return ()

        FormFailure errs -> do
            liftIO $ putStrLn "Post project form"
            print errs

    defaultLayout $ do
        setTitle $ toHtml $ projectTitle project
        $(widgetFile "project")

deleteProjectR :: ProjectId -> Handler ()
deleteProjectR projectId = do
    mp <- runDB $ get projectId
    for_ mp $ \p -> do
        runDB $ delete projectId
        deleteImage $ projectIcon p
        setMessage "Project deleted successfully"
        sendResponse ("Project deleted successfully" :: Text)
    sendResponseStatus status404 ("Project does not exist" :: Text)

pFormIdent :: Text
pFormIdent = "project-form"

getNewProject :: Widget
getNewProject = do
    pForm <- liftHandler $
        genFormIdentify pFormIdent $ projectForm Nothing
    mkModal "New Project" pForm

postNewProject :: Widget
postNewProject = do
    ((pResult, pWidget'), pEnctype) <- liftHandler $
        runFormIdentify pFormIdent $ projectForm Nothing

    case pResult of
        FormSuccess project -> do
            pId <- liftHandler $ runDB $ insert project
            redirect $ ProjectR pId

        FormMissing -> return ()

        FormFailure errs -> do
            liftHandler $ liftIO $ putStrLn "postNewProject"
            liftHandler $ print errs

    let pForm = (pWidget', pEnctype)
    
    mkModal "New Project" pForm

projectForm :: Maybe Project -> Form Project
projectForm mp extra = do
    (titleRes, titleView) <- mreq textField defs (projectTitle <$> mp)
    (urlRes, urlView) <- mreq pUrlField defs (projectUrl <$> mp)
    (pubRes, pubView) <- mreq checkBoxField (withClass "lg-checkbox" "") (projectPublished <$> mp)
    (mFileRes, iconView) <- case mp of
        Nothing -> fmap (first (fmap Just)) $ mreq fileField uploadSettings Nothing
        Just p -> mopt fileField uploadSettings Nothing

    mIconRes <- lift $ traverse (traverse uploadImage) mFileRes

    let iconRes = fmap (fromMaybe (projectIcon $ fromJust mp)) mIconRes
        contentRes = pure $ maybe [] projectContent mp
        projectRes = Project <$> titleRes <*> urlRes <*> pubRes <*> iconRes <*> contentRes
        projectWidget =
            [whamlet|
                #{extra}
                <div .form-group>
                    <label for=#{fvId titleView}>Title
                    ^{fvInput titleView}
                <div .form-group>
                    <label for=#{fvId urlView}>Url
                    ^{fvInput urlView}
                    <small #urlHelp .form-text .text-muted>#{urlTip}
                <div .form-group>
                    <label for=#{fvId pubView}>Published
                    ^{fvInput pubView}
                    <small #pubHelp .form-text .text-muted>#{pubTip}
                <div .form-group>
                    <label for=#{fvId iconView}>Icon
                    ^{fvInput iconView}
                    <small #iconHelp .form-text .text-muted>#{uploadTip}
            |]

    -- if form failed but we uploaded an image, delete it
    case projectRes of
        FormFailure _ ->
            case iconRes of
                FormSuccess imgId -> do
                    _ <- lift $ deleteImage imgId
                    return ()
                _ -> return ()

        _ -> return ()

    return (projectRes, projectWidget)

    where
        defs = withClass "form-control" ""
        urlTip = "The project will appear at <your domain>/projects/<url>. Only letters, numbers, hyphens and underscores are permitted." :: Text
        pUrlField = check validateUrl textField
        pubTip = "Projects that are not published will only be viewable by admins." :: Text
        uploadTip = "This image will be shown as the thumbnail for the project on the homepage." :: Text
        uploadSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("accept", ".jpg, .png, .gif")
                , ("class", "form-control-file") ]
            }
        validateUrl t =
            if isValid
                then Right t
                else Left err
            where
                err :: Text
                err = "Please enter a value containing only letters, numbers, hyphens and underscores)."
                isValid = T.foldr (\c b -> b && c `elem` validChars) True t
                validChars = '-' : '_' : (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])