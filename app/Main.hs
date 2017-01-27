{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class  (liftIO)
import Lib (getMenus)
import Components (mainPage, PageState (PageState))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty


main :: IO ()
main = scotty 3000 $ do
    get "/" showMenus


showMenus :: ActionM ()
showMenus = liftIO getMenus >>= \menus ->
  html . renderHtml $ mainPage (PageState "page1" menus)
