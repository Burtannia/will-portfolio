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
    (titleRes, titleView) <- mreq textField defs (projectTitle <$> mp)
    (urlRes, urlView) <- mreq pUrlField defs (projectUrl <$> mp)
    (pubRes, pubView) <- mreq checkBoxField (withClass "lg-checkbox" "") (projectPublished <$> mp)
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