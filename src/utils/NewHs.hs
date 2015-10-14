{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  August 21, 2015

  version 0.0.1

  Create new .hs/.lhs file with import Module section loaded.

  The format will be:

  NewHs [hs/lhs] [folder/name]
-}

module Main
where

import qualified System.Environment as Sys
import qualified Safe as Safe
import qualified Data.Text.Lazy as T
import Data.Maybe

main = do
  instructions
  parameters <- interactive
  test parameters
  process parameters

instructions = putStrLn "NewHs: Haskell file template.\n\
                        \  usage: NewHs [file type (hs/lhs)] [file name] \n\
                        \  example: NewHs hs ./Main.hs \n"

interactive = do
  args <- Sys.getArgs
  if length args /= 2
    then return (Nothing, Nothing)
    else
      let filetype = Just (head args) :: Maybe String
          filename = Just (head $ drop 1 args) :: Maybe String
      in return (filetype, filename)

test (filetype, filename) 
  | and [filetype == Nothing, filename == Nothing] = putStrLn "Not correct input format\n"
  | filetype == Nothing = putStrLn "Error: Select either 'hs' or literal haskell 'lhs'.\n"
  | filename == Nothing = putStrLn "Error: The Filename is not in good format.\n"
  | otherwise = putStrLn "Everything looks good.\n"
  
process (filetype, filename)
  | or [filetype == Nothing, filename == Nothing] = putStrLn "Error in input."
  | filetype == Just "hs" = writeFile (fromJust filename) (unlines hsFormat)
  | filetype == Just "lhs" = writeFile (fromJust filename) (unlines lhsFormat)                           

lhsFormat =
  ["> {-#LANGUAGE OverloadedStrings#-}"
  , ""
  , "{-"
  , "Project name: "
  , "Min Zhang"
  , "Date: "
  , "Version: "
  , "README: "
  , "-}"
  , ""
  , "> import qualified Data.Text.Lazy as T" 
  , "> import qualified Data.Text.Lazy.IO as TextIO"
  , "> import qualified Data.Char as C"
  , "> import Control.Applicative"
  , "> import qualified Data.List as L"
  , "> import Control.Monad (fmap)"
  , "> import Data.Ord (comparing)"
  , "> import Data.Function (on)"
  , "> import qualified Safe as S"
  , "> import qualified Data.HashMap.Lazy as M"
  , "> import qualified Data.Maybe as Maybe"
  , "> import qualified Data.Foldable as F (all)"
  , "> import Data.Traversable (sequenceA)"
  , "> import qualified Data.Dates as Dates"
  , "> import qualified Data.ByteString.Lazy.Char8 as Bl"
  , "> import qualified System.IO as IO"
  , "> import System.Environment"
  , "> import System.Directory"
  , "> import MyText"
  , "> import MyTable"
  , "> import Util"]

hsFormat = map (T.unpack . T.replace "> " "" . T.pack) lhsFormat

