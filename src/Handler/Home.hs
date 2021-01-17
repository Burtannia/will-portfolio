{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Handler.Project
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4, bfs)
import Text.Julius (RawJS (..))

getHomeR :: Handler Html
getHomeR = do
    (pWidget, pEnctype) <- genFormIdentify pFormIdent $ projectForm Nothing
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((pResult, pWidget), pEnctype) <- runFormIdentify pFormIdent $ projectForm Nothing
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")