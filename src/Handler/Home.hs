{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Handler.Modal
import Handler.Project
import Handler.Image
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4, bfs)
import Text.Julius (RawJS (..))

theTitle :: Html
theTitle = toHtml
    ("Will Burton - Gamer & Hobbyist Map Designer" :: Text)

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuth
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        npWidget = getNewProject

    projects <- getAllProjects

    defaultLayout $ do
        setTitle theTitle
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    muser <- maybeAuth
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        npWidget = postNewProject

    projects <- getAllProjects
    
    defaultLayout $ do
        setTitle theTitle
        $(widgetFile "homepage")

getAllProjects :: Handler [Entity Project]
getAllProjects = runDB $
    selectList [ProjectPublished ==. True] [Asc ProjectTitle]