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
    let npWidget = getNewProject
    defaultLayout $ do
        setTitle theTitle
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    let npWidget = postNewProject
    defaultLayout $ do
        setTitle theTitle
        $(widgetFile "homepage")