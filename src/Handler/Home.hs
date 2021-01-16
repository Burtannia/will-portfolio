{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4, bfs)
import Text.Julius (RawJS (..))

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")