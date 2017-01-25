{-# LANGUAGE OverloadedStrings #-}

module Components
  (
    mainPage
  , PageState (PageState)
  ) where

import Control.Monad (forM_)
import Lib (Menu (Menu))
import Text.Blaze.Html5 hiding (html, param, main)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H

data PageState = PageState {
  title :: Html,
  menus :: [Menu]
}

-- template
mainPage :: PageState -> Html
mainPage (PageState title menus)  = H.html $ do
  H.head $
    H.title title
  H.body $
    renderMenus menus
    --H.ul $ mapM_ H.li menus

renderMenus :: [Menu] -> Html
renderMenus menus = forM_ menus renderMenu

renderMenu :: Menu -> Html
renderMenu (Menu who options) = H.html $ do
  H.div $ do
    H.h1 (toHtml who)
    H.ul $
      forM_ options (H.li . toHtml)
