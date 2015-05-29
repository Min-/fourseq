{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  5-29-2015

  import tables, mostly csv and txt, to list of lists

  all the text will be in Text type
-}

module Tables
  ( importTable
  , smartTable
  )
where

import qualified Data.Text as T 
import qualified Data.Text.IO as TextIO
import qualified Data.Char as C
import Control.Applicative
import qualified Data.Map as M

import qualified Data.ByteString.Lazy.Char8 as Bl
import qualified System.IO as IO

import MyText

-- if the table is windows format, then need to remove quotations too
importTable delim input = 
  if delim == "\t" 
  then do map (tab . cleanWinNl . cleanQuote) . T.lines <$> TextIO.readFile input
  else if delim == ","
       then do  map (comma . cleanWinNl . cleanQuote) . T.lines <$> TextIO.readFile input
       else return []

-- use lazy bytestring to import first few lines (or first few bytes) of the file, not to overload the memory, just to text that type of table the import is. Therefore, to ease the use of importTable. Not sure if it's a good idea or not. Need to test it out.

smartTable input = do
  inhandle <- IO.openFile input IO.ReadMode
  first1k <- Bl.hGet inhandle 1000 
  let t = Bl.count '\t' first1k
  let c = Bl.count ',' first1k
  let r = Bl.count '\r' first1k
  if t > c
  then do importTable "\t" input
  else do importTable "," input
  
