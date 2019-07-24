{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (putStrLn)
import Data.Foldable (find)
import Data.Maybe (maybe)
import System.Environment (getEnv)
import qualified Xeno.DOM as XML
import Prelude hiding (putStrLn)

data Leather = Leather
  { lName  :: ByteString
  , lSharp :: ByteString
  , lBlunt :: ByteString
  , lHeat  :: ByteString
  , lICold :: ByteString
  , lIHeat :: ByteString
  }

columns :: [(Int, ByteString)]
columns = [(20, "NAME"), (8, "SHARP"), (8, "BLUNT"), (8, "HEAT"), (8, "ICOLD"), (8, "IHEAT")]

fill :: Int -> ByteString -> ByteString
fill w bs = bs <> spaces (w - BS.length bs)
  where
    spaces w' | w' <= 0   = ""
              | otherwise = " " <> spaces (w' - 1)

printLeather :: Leather -> IO ()
printLeather (Leather name sharp blunt heat icold iheat) =
  putStrLn $ mconcat $ fmap (uncurry fill) $ zip (fmap fst columns) [name, sharp, blunt, heat, icold, iheat]

printLeathers :: [Leather] -> IO ()
printLeathers ls = do
  putStrLn $ mconcat $ fmap (uncurry fill) columns
  mapM_ printLeather ls

xmlPath :: IO FilePath
xmlPath =
  fmap (<> "/.local/share/Steam/steamapps/common/RimWorld/Mods/Core/Defs/ThingDefs_Items/Items_Resource_Stuff_Leather.xml")
       (getEnv "HOME")

main :: IO ()
main = do
  xml <- BS.readFile =<< xmlPath
  case XML.parse xml of
    Left err -> print err
    Right root -> printLeathers $ foldr makeLeathers [] $ XML.contents root
  where
    makeLeathers :: XML.Content -> [Leather] -> [Leather]
    makeLeathers (XML.Element e) all
      | XML.name e == "ThingDef" = if isLeather e
                                   then leather e : all
                                   else all
    makeLeathers _  all = all

    isLeather :: XML.Node -> Bool
    isLeather e = ("ParentName", "LeatherBase") `elem` attrs || ("Name", "LeatherBase") `elem` attrs
      where
        attrs = XML.attributes e

    leather :: XML.Node -> Leather
    leather node = Leather name (stat "StuffPower_Armor_Sharp")
                                (stat "StuffPower_Armor_Blunt")
                                (stat "StuffPower_Armor_Heat")
                                (stat "StuffPower_Insulation_Cold")
                                (stat "StuffPower_Insulation_Heat")
      where
        name :: ByteString
        name = maybe "Leather_Base"
               (text . head . XML.contents)
               (find (\n -> XML.name n == "defName") $ XML.children node)

        stat :: ByteString -> ByteString
        stat key = maybe ""
                    (\statBases -> maybe ""
                                   (text . head . XML.contents)
                                   (find ((== key) . XML.name) $ XML.children statBases))
                    (find (\n -> XML.name n == "statBases") $ XML.children node)

        text :: XML.Content -> ByteString
        text (XML.Text t) = t
        text _ = ""