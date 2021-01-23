{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Project where

import Import

import Handler.Image
import Handler.Modal
import Handler.Component

import Data.Aeson.Types
import Text.Julius (RawJS (..))
import qualified Data.Text as T (foldr)
import Data.Maybe (fromJust)
import qualified Data.List as L ((!!))

getProjectR :: ProjectId -> Handler Html
getProjectR projectId = do
    muser <- maybeAuth
    project <- runDB $ get404 projectId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = projectPublished project

    when (not isAdmin && not published) notFound

    pForm <- genFormIdentify pFormIdent $ projectForm $ Just project

    let pWidget = mkModal "Edit Project" pForm
        newCompWidget = getNewComponent
        comps = withIndexes $ projectContent project
        compWidgets = map (uncurry $ getComponent projectId) comps

    defaultLayout $ do
        setTitle $ toHtml $ projectTitle project
        $(widgetFile "project")

postProjectR :: ProjectId -> Handler Html
postProjectR projectId = do
    muser <- maybeAuth
    ep <- runDB $ getEntity projectId >>= maybe notFound return

    let project = entityVal ep
        isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = projectPublished project

    when (not isAdmin && not published) notFound

    ((pResult, pWidget'), pEnctype) <- runFormIdentify pFormIdent $ projectForm $ Just project

    let pForm = (pWidget', pEnctype)
        pWidget = mkModal "Edit Project" pForm
        newCompWidget = postNewComponent ep
        comps = withIndexes $ projectContent project
        compWidgets = map (uncurry $ postComponent ep) comps

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

data ProjectUpdate
    = DeleteComp Int
    | CompUp Int
    | CompDown Int
    deriving (Show, Read, Generic)

instance ToJSON ProjectUpdate where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ProjectUpdate where
    parseJSON = genericParseJSON defaultOptions

patchProjectR :: ProjectId -> Handler ()
patchProjectR projectId = do
    mp <- runDB $ get projectId
    pUpdate <- requireCheckJsonBody :: Handler ProjectUpdate

    for_ mp $ \p -> do
        let cs = projectContent p

        case pUpdate of
            DeleteComp ix
                | ix >= 0 && ix < length cs -> do
                    runDB $ update projectId $
                        [ ProjectContent =. cs -! ix ]
                    deleteComponent $ cs L.!! ix
                    sendResponse ("Project updated" :: Text)
                | otherwise -> sendResponseStatus status500
                    ("Index out of bounds " <> tshow ix)

            CompUp ix
                | ix >= 0 && ix < length cs -> do
                    runDB $ update projectId $
                        [ ProjectContent =. moveIxLeft ix cs ]
                    sendResponse ("Project updated" :: Text)
                | otherwise -> sendResponseStatus status500
                    ("Index out of bounds " <> tshow ix)

            CompDown ix
                | ix >= 0 && ix < length cs -> do
                    runDB $ update projectId $
                        [ ProjectContent =. moveIxRight ix cs ]
                    sendResponse ("Project updated" :: Text)
                | otherwise -> sendResponseStatus status500
                    ("Index out of bounds " <> tshow ix)

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
    (iconRes, iconView) <- mreq imageField imageSettings (projectIcon <$> mp)

    let contentRes = pure $ maybe [] projectContent mp
        projectRes = Project <$> titleRes <*> urlRes <*> pubRes <*> iconRes <*> contentRes
        projectWidget =
            [whamlet|
                #{extra}
                ^{mkWidgetBs4 titleView "Title" Nothing}
                ^{mkWidgetBs4 urlView "Url" $ Just urlTip}
                ^{mkWidgetBs4 pubView "Published" $ Just pubTip}
                ^{mkWidgetBs4 iconView "Icon" $ Just uploadTip}
            |]

    -- if form failed but we saved a new image, delete it
    case projectRes of
        FormFailure _ ->
            case iconRes of
                FormSuccess imgId ->
                    when (Just imgId /= fmap projectIcon mp) $
                        lift $ deleteImage imgId
                _ -> return ()

        _ -> return ()

    return (projectRes, projectWidget)

    where
        defs = withClass "form-control" ""
        urlTip = "The project will appear at <your domain>/projects/<url>. Only letters, numbers, hyphens and underscores are permitted." :: Text
        pUrlField = check validateUrl textField
        pubTip = "Projects that are not published will only be viewable by admins." :: Text
        uploadTip = "This image will be shown as the thumbnail for the project on the homepage." :: Text
        validateUrl t =
            if isValid
                then Right t
                else Left err
            where
                err :: Text
                err = "Please enter a value containing only letters, numbers, hyphens and underscores)."
                isValid = T.foldr (\c b -> b && c `elem` validChars) True t
                validChars = '-' : '_' : (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])