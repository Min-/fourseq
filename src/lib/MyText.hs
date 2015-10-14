{-#LANGUAGE OverloadedStrings#-}

{-
  Routine functions for Data.Text manipulation.

  Min Zhang

  Feb 6, 2015

  v.0.1.1 (Oct 9, 2015): add Type signature to all functions
  v.0.1.2 (Oct 14, 2015): switch from strict Data.Text to Data.Text.Lazy
-}

module MyText
    ( capitalize
    , range
    , toInt
    , toDouble
    , readInt
    , readDouble
    , chop
    , clear
    , isEmpty
    , squeeze
    , tab
    , comma
    , untab
    , uncomma
    , cleanWinNl
    , cleanQuote
    , eachLine
    , countElem
    , countWords
    , countLetter
    )
where

import qualified Data.Text.Lazy as T 
import qualified Data.Char as C
import qualified Data.Map as M
import qualified GHC.Int as GHC.Int

capitalize :: T.Text -> T.Text
capitalize x = T.cons (C.toUpper $ T.head x) (T.toLower $ T.tail x)

--Indexing
range :: Integral a => (GHC.Int.Int64, a) -> T.Text -> T.Text
range (s, e) = T.take ((fromIntegral e) - (fromIntegral s) + 1 ) . T.drop s

--read and show numbers
toInt :: T.Text -> Int
toInt = read . T.unpack

toDouble :: T.Text -> Double
toDouble = read . T.unpack

readInt :: Int -> T.Text
readInt = T.pack . show

readDouble :: Double -> T.Text
readDouble = T.pack . show 

-- 
chop :: T.Text -> T.Text
chop "" = ""
chop t
  |T.last t == '\n' = if T.last (T.init t) == '\r' then (T.init $ T.init t) else T.init t
  |otherwise = T.init t

clear :: T.Text -> T.Text
clear _ = ""

isEmpty :: T.Text -> Bool
isEmpty t =
  case t of 
    "" -> True
    _  -> False

squeeze :: T.Text -> T.Text -> T.Text
squeeze t s = 
  if T.isInfixOf d s 
  then squeeze t (T.replace d t s)
  else s
  where d = T.replicate 2 t

-- separations
tab :: T.Text -> [T.Text]
tab = T.splitOn "\t"

untab :: [T.Text] -> T.Text
untab = T.intercalate "\t"

comma :: T.Text -> [T.Text]
comma = T.splitOn ","

uncomma :: [T.Text] -> T.Text
uncomma = T.intercalate ","

cleanWinNl :: T.Text -> T.Text
cleanWinNl = T.replace "\r" ""

cleanQuote :: T.Text -> T.Text
cleanQuote = T.replace "\"" ""

-- high order
eachLine :: (T.Text -> T.Text) -> T.Text -> T.Text
eachLine f = T.unlines . map f . T.lines

countElem :: (Ord k, Num a) => (k -> Bool) -> [k] -> [(k, a)]
countElem f e = M.toList $ M.fromListWith (+) [(c, 1) | c <- input]
                 where input = filter f e
       
countChar :: Num a => T.Text -> [(Char, a)]
countChar t = countElem allTrue input
            where input = T.unpack t

countWords :: Num a => T.Text -> [(T.Text, a)]
countWords s = countElem allTrue input
            where input = T.words s

countLetter :: Num a => T.Text -> [(Char, a)]
countLetter t = countElem C.isLetter input
            where input = T.unpack t

allTrue :: a -> Bool
allTrue _ = True

