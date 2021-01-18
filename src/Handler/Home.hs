{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Handler.Modal
import Handler.Project
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4, bfs)
import Text.Julius (RawJS (..))

theTitle :: Html
theTitle = toHtml
    ("Will Burton - Gamer & Hobbyist Map Designer" :: Text)

getHomeR :: Handler Html
getHomeR = do
    pForm <- genFormIdentify pFormIdent $ projectForm Nothing
    let pWidget = mkModal "New Project" pForm
    defaultLayout $ do
        setTitle theTitle
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((pResult, pWidget'), pEnctype) <- runFormIdentify pFormIdent $ projectForm Nothing
    let pForm = (pWidget', pEnctype)
        pWidget = mkModal "New Project" pForm
    defaultLayout $ do
        setTitle theTitle
        $(widgetFile "homepage")