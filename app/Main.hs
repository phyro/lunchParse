{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class  (liftIO)
import Lib
import Components (mainPage, PageState (PageState))
import Text.Blaze.Html5 hiding (html, param, main)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Web.Scotty


main :: IO ()
main = scotty 3000 $ do
    get "/" showMenus


showMenus :: ActionM ()
showMenus = liftIO getMenus >>= \menus ->
  html . renderHtml $ mainPage (PageState "page1" menus)
