{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  Oct 14, 2015
  
  Functions on DNA sequences

  v.0.1.0 (Oct 14, 2015): switch from Data.Text to Data.Text.Lazy 
-}



module Dna
  (  copySeq
   , complSeq
   , revSeq
   , revCompSeq
   , dnaToRna
   , gcPct
  )
where

import qualified Data.Text.Lazy as T
import Data.List (genericLength)

--with copySeq together, clean up DNA input, remove not ATGC, turn everything into uppercase 
toDna = T.toUpper

copySeq = T.map copyNt . toDna
  where 
    copyNt nt = case nt of 
      'A' -> 'A'
      'T' -> 'T' 
      'G' -> 'G' 
      'C' -> 'C'
      _   -> 'N'

complSeq = T.map complNt . toDna
  where
    complNt nt = case nt of
      'A' -> 'T'
      'T' -> 'A'
      'G' -> 'C'
      'C' -> 'G'
      _   -> 'N'

revSeq = copySeq . T.reverse . toDna

revCompSeq = revSeq . complSeq . toDna

dnaToRna = T.map dnaToRnaNt . toDna
  where
    dnaToRnaNt nt = case nt of 
      'T' -> 'U'
      _   -> nt   -- save a lot of lines 

gcPct seq = lenGC seq / (realToFrac $ T.length seq)
  where
    lenGC = T.foldr ((+) . countGC) 0 . copySeq
    countGC nt = case nt of
      'G' -> 1.0
      'C' -> 1.0
      _   -> 0

--need a function to test if the input format is good, or put it into IO? or it's too costful for data from fastq? (because most of the format actually is good in fastq file)


