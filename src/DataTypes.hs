{-#LANGUAGE OverloadedStrings#-}
{-translation from Julia code, for several purposes
 1) what we can do better
 2) what is Julia code good at
 3) what is Haskell code good at
 4) revise and documentate the codes
 5) better error handling, complete program
 6) similarity Julia learnt from Haskell

-- copy from IonTorrent, template for 4C-Seq data analysis
-- 11-25-14 modified add Fasta for 4C analysis
-}

module DataTypes
where

import qualified Data.Text as T

data Sequence = Sequence

data Reads = Reads
  {  qname	::T.Text
   , flag	::Int
   , rname	::T.Text
   , pos	::Int
   , mapq	::Int
   , cigar	::T.Text
   , rnext	::T.Text
   , pnext	::T.Text
   , tlen	::Int
   , samseq	::T.Text
   , qual	::T.Text
   , md		::T.Text
  } deriving (Show, Read, Eq)

data Barcodes =  Barcodes 
  {  name	::T.Text
   , bcseq	::T.Text
   , dir	::Strand  -- new type
   , len	::Int
   , rev	::T.Text
   , comp	::T.Text
   , rc		::T.Text
  } deriving (Eq, Show, Read)

data Strand = Fwd | Rvs
  deriving (Show, Read, Eq)

data Header = Header [T.Text]

data Samfile = Sam Header [Reads]

type Dna = T.Text

data CodeMode = Cigar | Md | Count | Seq | Flag | Len
  deriving (Eq, Show, Read)

-- add fasta type for 4C
data Fasta = Fasta 
  { faname :: T.Text
    ,faseq  :: T.Text
  } deriving (Eq, Show, Read)

data Fastq = Fastq
  { fqname :: T.Text
   ,fqseq :: T.Text
   ,fqstrand :: T.Text
   ,fqqual :: T.Text
  } deriving (Eq, Show, Read)

class Countable a where
  count :: a->Int

instance Countable Reads where
  count = T.length . samseq

instance Countable Fasta where
  count = T.length . faseq

instance Countable Barcodes where
  count = len
