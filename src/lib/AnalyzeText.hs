{-#LANGUAGE OverloadedStrings#-}

{-
Project name: AnalyzeText
Min Zhang
Date: Oct 14, 2015
Version: v.0.1.0
README: Taking a file and analyze what type it is.
-}

module AnalyzeText
where

import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Data.Char as C
import Control.Applicative
import qualified Data.List as L
import Control.Monad (fmap)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Safe as S
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as F (all)
import Data.Traversable (sequenceA)
import qualified Data.ByteString.Lazy.Char8 as Bl
import qualified System.IO as IO
import System.Environment
import System.Directory
import MyText
import MyTable
import Util
