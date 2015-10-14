{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  Oct 14, 2015

  Revise Barcodes codes

  v.0.1.0 (Oct 14, 2015) 
-}



module Barcodes
where

import DataTypes
import Dna
import qualified Data.Text as T

-- only use first 10 nt as primer to minimumize mismatch
constBC :: (T.Text, T.Text, Strand) -> Barcodes
constBC (name, bcseq, direction) = 
  let bc10 = T.take 10 bcseq
  in Barcodes name (copySeq bc10) direction 10 (revSeq bc10) (complSeq bc10) (revCompSeq bc10)

-- !!bc1 bc2 here are sequences, T.Text; not barcodes; this is a key helper function for matching 
matchBC r bc1 bc2 = headPlusTail == bc1PlusBc2
  where headPlusTail = T.concat [T.take (T.length bc1) (samseq r),  (T.reverse . T.take (T.length bc2) . T.reverse) (samseq r)] 
        bc1PlusBc2 = T.concat [bc1, bc2]

-- filtersample in julia; combine forward matching and reverse matching
filterMatchedReads rs bc1 bc2 = filter (\r->matchBC r (bcseq bc1) (rc bc2)) rs ++ filter (\r->matchBC r (bcseq bc2) (rc bc1)) rs        

-- batchfindsample in julia
findAllBC = undefined
  
