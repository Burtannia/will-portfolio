{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Import.Utils where

import Import.NoFoundation
import Foundation
import qualified Data.List as L (tail, init)
import Data.Time.Clock (NominalDiffTime)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)
import Yesod.Form.MultiInput

imageSettings :: FieldSettings App
imageSettings = FieldSettings
    { fsLabel = ""
    , fsTooltip = Nothing
    , fsId = Nothing
    , fsName = Nothing
    , fsAttrs =
        [ ("accept", ".jpg, .png, .gif")
        , ("class", "form-control-file") ]
    }

mkWidgetBs4 :: FieldView App -> Text -> Maybe Text -> Widget
mkWidgetBs4 theView theLabel mtt =
    [whamlet|
        $with hasErr <- isJust $ fvErrors theView
            <div .form-group :hasErr:.has-error>
                $if theLabel /= ""
                    <label for=#{fvId theView}>#{theLabel}
                ^{fvInput theView}
                $maybe err <- fvErrors theView
                    <div .invalid-feedback .d-block>#{err}
                $maybe tt <- mtt
                    <small .form-text .text-muted>#{tt}
    |]

mkMvWidgetBs4 :: MultiView App -> Widget
mkMvWidgetBs4 mv =
    [whamlet|
        $with hasErr <- isJust $ fvErrors $ mvAddBtn mv
            <div.form-group :hasErr:.has-error>
                ^{fvInput $ mvCounter mv}

                $forall fv <- mvFields mv
                    ^{helper fv}

                ^{helper $ mvAddBtn mv}
    |]
    where
        helper fv =
            [whamlet|
                ^{fvInput fv}

                $maybe err <- fvErrors fv
                    <div .invalid-feedback .d-block>#{err}

                $maybe tt <- fvTooltip fv
                    <small .form-text .text-muted>#{tt}
            |]

sequence2 :: Monad m => (m a, b) -> m (a, b)
sequence2 = fmap swap . sequence . swap

withClass :: Text -> FieldSettings App -> FieldSettings App
withClass t fs = fs {fsAttrs = addClass t $ fsAttrs fs}

formatDiffTime :: NominalDiffTime -> String
formatDiffTime dt = go (floor $ toRational dt) incs
    where
        go :: Int -> [(Int, String, String)] -> String
        go _ [] = "formatDiffTime: this should never happen"
        go t ((x, sing, plur) : xs) =
            case t `divMod` x of
                (0, 1) -> "1" ++ sing
                (0, n) -> show n ++ plur
                (m, _) -> go m xs
        incs :: [(Int, String, String)]
        incs =
            [ (60, " second", " seconds")
            , (60, " minute", " minutes")
            , (24, " hour", " hours")
            , (30, " day", " days")
            , (12, " month", " months")
            , (1, " year", " years")
            ]

moveForward :: Eq a => a -> [a] -> [a]
moveForward _ [] = []
moveForward _ [x] = [x]
moveForward x (y:z:ys)
    | x == y    = z : y : ys
    | otherwise = y : moveForward x (z:ys)

moveBackward :: Eq a => a -> [a] -> [a]
moveBackward _ [] = []
moveBackward _ [x] = [x]
moveBackward x (y:z:ys)
    | x == z    = z : y : ys
    | otherwise = y : moveBackward x (z:ys)

moveIxRight :: Int -> [a] -> [a]
moveIxRight n = map snd . go . withIndexes
    where
        go [] = []
        go [x] = [x]
        go (x@(m, _) : y : xs)
            | n == m = y : x : xs
            | otherwise = x : go (y:xs)

moveIxLeft :: Int -> [a] -> [a]
moveIxLeft n = map snd . go . withIndexes
    where
        go [] = []
        go [x] = [x]
        go (x : y@(m, _) : xs)
            | n == m = y : x : xs
            | otherwise = x : go (y:xs)

topTail :: [a] -> [a]
topTail xs
    | length xs < 2 = xs
    | otherwise = L.init $ L.tail xs

mkFormId :: [Text] -> Text
mkFormId ts = foldr (<>) "" $ intersperse "" ts

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

fours :: [a] -> [[a]]
fours xs
    | length xs < 4 = [xs]
    | otherwise = take 4 xs : fours (drop 4 xs)

withIndexes :: [a] -> [(Int, a)]
withIndexes xs = zip [0..] xs

(-!) :: [a] -> Int -> [a]
(-!) xs n = [ x | (i,x) <- withIndexes xs, not $ i == n ]

(-=!) :: Eq a => [a] -> a -> [a]
(-=!) xs y = [x | x <- xs, not $ x == y]

(/!) :: [a] -> (a, Int) -> [a]
(/!) xs (y, n) = [ if i == n then y else x | (i,x) <- withIndexes xs ]

mkOptions :: Text -> [(Text, a)] -> OptionList a
mkOptions prefix xs = mkOptionList opts
    where
        mkOption (ix, (disp, val)) = Option disp val $ prefix <> tshow ix
        opts = map mkOption $ withIndexes xs

isSuccess :: FormResult a -> Bool
isSuccess (FormSuccess _) = True
isSuccess _ = False

boundsCheck :: (Int -> [a] -> [a]) -> [a] -> Int -> [a]
boundsCheck f xs n
    | n < 0 = xs
    | n >= length xs = xs
    | otherwise = f n xs

boundsCheckM :: Monad m => [a] -> Int -> m b -> m (Either Text b)
boundsCheckM xs n mb
    | n < 0 = return $ Left "Index out of bounds (negative)"
    | n >= length xs = return $ Left "Index out of bounds"
    | otherwise = liftM Right mb
    
genFormIdentify :: Text -> Form a -> Handler (Widget, Enctype)
genFormIdentify t = generateFormPost . identifyForm t

runFormIdentify :: Text -> Form a -> Handler ((FormResult a, Widget), Enctype)
runFormIdentify t = runFormPost . identifyForm t

genBs4Form :: AForm Handler a -> Handler (Widget, Enctype)
genBs4Form = genBs4Form' BootstrapBasicForm

runBs4Form :: AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4Form = runBs4Form' BootstrapBasicForm

genBs4Form' :: BootstrapFormLayout -> AForm Handler a -> Handler (Widget, Enctype)
genBs4Form' formType = generateFormPost . renderBootstrap4 formType

runBs4Form' :: BootstrapFormLayout -> AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4Form' formType = runFormPost . renderBootstrap4 formType

genBs4FormIdentify :: Text -> AForm Handler a -> Handler (Widget, Enctype)
genBs4FormIdentify = genBs4FormIdentify' BootstrapBasicForm

runBs4FormIdentify :: Text -> AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4FormIdentify = runBs4FormIdentify' BootstrapBasicForm

genBs4FormIdentify' :: BootstrapFormLayout
    -> Text
    -> AForm Handler a
    -> Handler (Widget, Enctype)
genBs4FormIdentify' formType t = generateFormPost . identifyForm t . renderBootstrap4 formType

runBs4FormIdentify' :: BootstrapFormLayout
    -> Text
    -> AForm Handler a
    -> Handler ((FormResult a, Widget), Enctype)
runBs4FormIdentify' formType t = runFormPost . identifyForm t . renderBootstrap4 formType

convertFieldPair :: (c -> a)
    -> (c -> b)
    -> (a -> b -> c)
    -> Field Handler a
    -> Field Handler b
    -> Text
    -> Field Handler c
convertFieldPair toA toB toC fa fb wrapClass = Field
    { fieldParse = \rawVals fileVals -> do
        let parseA = fieldParse fa
            parseB = fieldParse fb

        eResA <- parseA rawVals fileVals
        eResB <- parseB (safeTail rawVals) (safeTail fileVals)

        return $ liftA2 (liftA2 toC) eResA eResB

    , fieldView = \ti tn as eRes req -> do
        let viewA = fieldView fa
            viewB = fieldView fb
        [whamlet|
            <div ##{ti} class=#{wrapClass}>
                ^{viewA (ti <> "-A") tn as (fmap toA eRes) req}
                ^{viewB (ti <> "-B") tn as (fmap toB eRes) req}
        |]
    , fieldEnctype = fieldEnctype fa
    }