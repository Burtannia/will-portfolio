{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Project where

import Import
import Text.Julius (RawJS (..))
import qualified Data.Text as T (foldr)
import Handler.Image
import Data.Maybe (fromJust)

getProjectR :: ProjectId -> Handler Html
getProjectR projectId = do
    muser <- maybeAuth
    project <- runDB $ get404 projectId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = projectPublished project

    when (not isAdmin && not published) notFound

    (pWidget, pEnctype) <- genFormIdentify pFormIdent $ projectForm $ Just project

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

    ((pResult, pWidget), pEnctype) <- runFormIdentify pFormIdent $ projectForm $ Just project

    case pResult of
        FormSuccess _ -> return ()

        FormMissing -> return ()

        FormFailure errs -> do
            liftIO $ putStrLn "Post project form"
            print errs

    defaultLayout $ do
        setTitle $ toHtml $ projectTitle project
        $(widgetFile "project")

deleteProjectR :: ProjectId -> Handler ()
deleteProjectR projectId = return ()

pFormIdent :: Text
pFormIdent = "project-form"

projectForm :: Maybe Project -> Form Project
projectForm mp extra = do
    (titleRes, titleView) <- mreq textField (fs "" Nothing) (projectTitle <$> mp)
    (urlRes, urlView) <- mreq pUrlField (fs "" urlTip) (projectUrl <$> mp)
    (pubRes, pubView) <- mreq boolField pubSettings (projectPublished <$> mp)
    (mFileRes, iconView) <- case mp of
        Nothing -> fmap (first (fmap Just)) $ mreq fileField uploadSettings Nothing
        Just p -> mopt fileField uploadSettings Nothing

    mIconRes <- lift $ traverse (traverse uploadImage) mFileRes

    -- TODO: rewrite without fromJust?
    let iconRes = fmap (fromMaybe (projectIcon $ fromJust mp)) mIconRes
        contentRes = pure $ maybe [] projectContent mp
        projectRes = Project <$> titleRes <*> urlRes <*> pubRes <*> iconRes <*> contentRes
        projectWidget =
            [whamlet|
                #{extra}
                ^{fvInput titleView}
                ^{fvInput urlView}
                ^{fvInput pubView}
                ^{fvInput iconView}
            |]

    return (projectRes, projectWidget)
    where
        urlTip = Just "The project will appear at <your domain>/projects/<url>. Only letters, numbers, hyphens and underscores are permitted."
        pUrlField = check validateUrl textField
        pubTip = Just "Projects that are not published will only be viewable by admins."
        uploadSettings = FieldSettings
            { fsLabel = "Icon"
            , fsTooltip = Just "This image will be used as a thumbnail if the guide is displayed on the homepage."
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("accept", ".jpg, .png, .gif")
                , ("class", "form-control-file") ]
            }
        pubSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = pubTip
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "lg-checkbox") ]
            }
        fs label mtt = FieldSettings
            { fsLabel = label
            , fsTooltip = mtt
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control") ]
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