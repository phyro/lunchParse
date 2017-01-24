module Lib
    ( someFunc
    ) where


import Data.List (intercalate)
import Data.Time (ZonedTime, defaultTimeLocale, getZonedTime, formatTime)
--import Data.Tree.NTree.TypeDefs (NTree)
import Text.XML.HXT.Core
import Text.HandsomeSoup

data DateRepr = DateRepr {
  dow :: Int,
  localTime :: ZonedTime
}

data Menu = Menu {
  who :: String,
  options :: [String]
}

--data Endpoint = Endpoint {
--  name :: String,
--  url :: String,
--  parser :: IOSArrow XmlTree (NTree XNode)
--}

instance Show Menu where
  show (Menu who options) = "who:" ++ who ++ "\n" ++ "menus:\n\t" ++
                            (intercalate "\n\t" options)

getDateRepr :: IO DateRepr
getDateRepr =
  getZonedTime >>= \localT ->
  let
    dayOfWeek = read (formatTime defaultTimeLocale "%w" localT) - 1 :: Int
  in
    return $ DateRepr dayOfWeek localT


--parseEndpoint :: Endpoint -> DateRepr -> IO Menu
--parseEndpoint (Endpoint name url parser) (DateRepr dow localTime) =
--  let
--    doc = fromUrl url
--  in
--    (runX $ (parser doc)) >>= \todayMenu ->
--    return $ Menu name todayMenu

--parserUnion doc =
--  doc
--  >>> css (".foodDayMenuBlock-" ++ show dow ++ " .foodItem")
--  -- take only the first menu
--  >>. take 1
--  >>> css (".foodItemDesc")
--  /> getText

parseUnion :: DateRepr -> IO Menu
parseUnion (DateRepr dow localTime) =
        let
          doc = fromUrl "http://www.pivnica-union.si/si/"
        in
          (runX $
            doc
            >>> css (".foodDayMenuBlock-" ++ show dow ++ " .foodItem")
            -- take only the first menu
            >>. take 1
            >>> css (".foodItemDesc")
            /> getText)
            >>= \todayMenu ->
          return $ Menu "Union" todayMenu
          --mapM_ putStrLn todayMenu

--getEndpoints :: [Endpoint]
--getEndpoints = [Endpoint "Union" "http://www.pivnica-union.si/si/" parserUnion]

getParsers :: [DateRepr -> IO Menu]
getParsers = [parseUnion]

someFunc :: IO ()
someFunc =
  getDateRepr >>= \dateRepr ->
  mapM (\parser -> parser dateRepr) getParsers >>= \menus ->
  --mapM (\endpoint -> parseEndpoint endpoint dateRepr) getEndpoints >>= \menus ->
  mapM_ print menus
