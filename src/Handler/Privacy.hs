{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Privacy where

import Import

getPrivacyR :: Handler Html
getPrivacyR = do
    defaultLayout $ do
        setTitle "Privacy - GDPR"
        $(widgetFile "privacy")