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
import Summernote
import Text.Julius (RawJS (..))

getNewComponent :: Widget
getNewComponent = do
    modalId <- newIdent
    let tabWidgets = map (uncurry $ genNewCompTab modalId) $ withIndexes blankComps
    $(widgetFile "components/new-component")
    where
        genNewCompTab modalId ix (compTitle, compId, cc) = do
            let isFirst = ix == 0
                formId = compId <> "-form"
            (formWidget, enctype) <- liftHandler $
                genFormIdentify formId $ compForm cc
            $(widgetFile "components/new-component-tab")

postNewComponent :: Entity Project -> Widget
postNewComponent ep = do
    modalId <- newIdent
    
    let tabWidgets = map (uncurry $ runNewCompTab modalId) $ withIndexes blankComps

    $(widgetFile "components/new-component")
    where
        runNewCompTab modalId ix (compTitle, compId, cc) = do
            let isFirst = ix == 0
                formId = compId <> "-form"
            
            ((formResult, formWidget), enctype) <- liftHandler $
                runFormIdentify formId $ compForm cc

            case formResult of
                FormSuccess comp -> do
                    let oldContent = projectContent $ entityVal ep

                    liftHandler $ runDB $
                        update (entityKey ep) [ ProjectContent =. oldContent ++ [comp] ]
                    
                    redirect $ ProjectR $ entityKey ep

                FormMissing -> return ()

                FormFailure errs -> do
                    liftIO $ putStrLn $ "postNewComponent: " <> compTitle
                    liftIO $ print errs

            $(widgetFile "components/new-component-tab")

getComponent :: ProjectId -> Int -> Component -> Widget
getComponent projectId ix c = do
    let formId = mkCompFormId projectId ix
    compEditForm <- liftHandler $
        genFormIdentify formId $ compForm $ toCreateComp c

    let compWidget = displayComp c
        compControls = mkCompControls projectId ix c compEditForm
    $(widgetFile "components/component-wrapper")

postComponent :: Entity Project -> Int -> Component -> Widget
postComponent ep ix c = do
    let projectId = entityKey ep
        formId = mkCompFormId projectId ix
    ((fRes, fWidget), fEnctype) <- liftHandler $
        runFormIdentify formId $ compForm $ toCreateComp c
    
    case fRes of
        FormSuccess comp -> do
            let oldContent = projectContent $ entityVal ep
            
            liftHandler $ runDB $ update projectId
                [ ProjectContent =. oldContent /! (comp, ix) ]

            redirect $ ProjectR projectId

        FormMissing -> return ()

        FormFailure errs -> do
            liftIO $ putStrLn $ "postComponent " <> tshow ix
            liftIO $ print errs

    let compEditForm = (fWidget, fEnctype)
        compWidget = displayComp c
        compControls = mkCompControls projectId ix c compEditForm
    $(widgetFile "components/component-wrapper")

mkCompFormId :: ProjectId -> Int -> Text
mkCompFormId projectId ix = toPathPiece projectId <> "-comp-" <> tshow ix

displayComp :: Component -> Widget
displayComp (C_ImageGroup imgIds) = do
    carouselId <- newIdent
    $(widgetFile "components/image-group")
displayComp (C_VideoEmbed url) = $(widgetFile "components/video-embed")
displayComp (C_Markup mId) = do
    mmarkup <- liftHandler $ runDB $ get mId
    for mmarkup $ \markup -> $(widgetFile "components/markup")
    return ()

mkCompControls :: ProjectId -> Int -> Component -> (Widget, Enctype) -> Widget
mkCompControls projectId ix c formBundle = do
    compUpId <- newIdent
    compDownId <- newIdent
    compDelId <- newIdent
    
    let editWidget = mkModal "Edit" formBundle

    $(widgetFile "components/controls")

toCreateComp :: Component -> CreateComp
toCreateComp (C_ImageGroup is) = CC_ImageGroup $ Just is
toCreateComp (C_VideoEmbed url) = CC_VideoEmbed $ Just url
toCreateComp (C_Markup mId) = CC_Markup $ Just mId

blankComps :: [(Text, Text, CreateComp)]
blankComps =
    [ ("Image Group", "image", CC_ImageGroup Nothing) 
    , ("Video Embed", "video", CC_VideoEmbed Nothing)
    , ("Markup", "markup", CC_Markup Nothing)
    ]

deleteComponent :: Component -> Handler ()
deleteComponent (C_ImageGroup is) = mapM_ deleteImage is
deleteComponent (C_Markup mId) = runDB $ delete mId
deleteComponent _ = return ()

data CreateComp
    = CC_ImageGroup (Maybe [ImageId])
    | CC_VideoEmbed (Maybe Text)
    | CC_Markup (Maybe MarkupBlockId)
    deriving Show

compForm :: CreateComp -> Form Component
compForm (CC_ImageGroup mis) extra = do  
    (imgRes, imgView) <- mreq multiImageField imageSettings mis

    let res = C_ImageGroup <$> imgRes
        wgt =
            [whamlet|
                #{extra}
                ^{mkWidgetBs4 imgView "Images" Nothing}
            |]
    
    return (res, wgt)

compForm (CC_VideoEmbed mt) extra = do
    (urlRes, urlView) <- mreq urlField (withClass "form-control" "") mt

    let res = C_VideoEmbed <$> urlRes
        wgt =
            [whamlet|
                #{extra}
                ^{mkWidgetBs4 urlView "Url" Nothing}
            |]
    
    return (res, wgt)

compForm (CC_Markup mmId) extra = do
    mmarkup <- lift $
        fmap (fmap markupBlockContent . join) $ -- Handler (Maybe Html)
        sequence $ -- Handler (Maybe (Maybe MarkupBlock))
        fmap (runDB . get) mmId -- Maybe (Handler (Maybe MarkupBlock))

    (markupRes', markupView) <- mreq snFieldUnsanitized (withClass "form-control" "") mmarkup

    let dbF = case mmId of
            Just mId -> \x -> repsert mId x >> return mId
            Nothing -> insert
    markupRes <- lift $ sequence $ fmap (runDB . dbF . MarkupBlock) markupRes'

    let res = C_Markup <$> markupRes
        wgt =
            [whamlet|
                #{extra}
                ^{mkWidgetBs4 markupView "Content" Nothing}
            |]

    return (res, wgt)