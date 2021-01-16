{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Project where

import Import
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4, bfs)
import Text.Julius (RawJS (..))

getProjectR :: ProjectId -> Handler Html
getProjectR projectId = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "project")

postProjectR :: ProjectId -> Handler Html
postProjectR projectId = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "project")