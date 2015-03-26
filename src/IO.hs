{-#LANGUAGE OverloadedStrings#-}

module IO
  ( importSamFile
  , samOutputFastq
  , samToFastq
  , samToFasta
  , importBarcodeTxt
  , importPairedBarcodes
  , outputSamFile
  , keepHeader
  , outputFasta
  , importFasta
  , importFastq
  , outputFastq)
where

import DataTypes
import Barcodes (constBC)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

--input samfile from system sam format file
importSamFile :: FilePath -> IO [Reads]
importSamFile f = readSam f >>= return . map constructReads . map splitFields . removeHeaders . T.lines
  where 
    readSam = TextIO.readFile 
    --removeHeaders = filter (\x->T.head x /= '@')
    removeHeaders = dropWhile ((=='@') . T.head)
    splitFields r = take 11 cols ++ (takeMd cols)
       where cols = T.words r
             takeMd x = let mdTag = filter (\x->T.take 5 x== "MD:Z:") x
                        in 
                          if null mdTag
                          then ["MD:Z:noMd"] --if there is no valid md tag of the read
                          else mdTag
    constructReads [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12] = 
      Reads r1 ((read . T.unpack) r2::Int) r3 ((read . T.unpack) r4 :: Int) ((read . T.unpack) r5 ::Int) r6 r7 r8 ((read . T.unpack) r9 ::Int) r10 r11 (T.drop 5 r12)  

keepHeader f = TextIO.readFile f >>= return . (T.intercalate "\n") . filter (\x->T.head x == '@') . T.lines

--ouput list of reads to sam format file
outputSamFile s filename = do
  let showReads (Reads qnm flg rnm pos mq cg rn pn tlen sseq q md) = T.intercalate "\t" [qnm, T.pack $ show flg, rnm, T.pack $ show pos, T.pack $ show mq, cg, rn, pn, T.pack $ show tlen, sseq, q] in TextIO.appendFile (filename::FilePath) (T.intercalate "\n" (map showReads s))

--for each Read, take the fields of fastq and combine by "\n", use foldl' for list add them together as a list of Data.Text
samOutputFastq :: FilePath -> [Reads] -> IO ()
samOutputFastq f rs = TextIO.writeFile (f::FilePath) fastq 
  where fastq = T.unlines $ map (\r-> T.concat ["@", (qname r), "\n", samseq r, "\n", "+", "\n",qual r]) rs

samOutputFasta :: FilePath -> [Reads] -> IO ()
samOutputFasta f rs = TextIO.writeFile (f::FilePath) fasta
  where fasta = T.unlines $ map (\r-> T.concat [">", (qname r), "\n", samseq r]) rs

--both sam file and fastq are system files 
samToFastq :: FilePath -> FilePath -> IO ()
samToFastq insam outfastq = importSamFile (insam::FilePath) >>= samOutputFastq (outfastq::FilePath) 

samToFasta :: FilePath -> FilePath -> IO ()
samToFasta insam outfasta = importSamFile (insam::FilePath) >>= samOutputFasta (outfasta::FilePath)

--import barcode from txt file
importBarcodeTxt f = TextIO.readFile f >>= return . map (constructBC . splitCols) . T.lines 
  where splitCols = T.splitOn "\t"
        constructBC [n, s, st] = constBC (n, s, (read $ T.unpack st)::Strand)  

importPairedBarcodes f = do
  bc <- importBarcodeTxt f
  let len = (length bc) `div` 2
  return $ [(x,y)| x<- (take len bc), y<- (drop len bc)]

importFasta filename = do
  TextIO.readFile filename >>= return . map (reconstructFasta . T.lines). tail . T.splitOn ">"
    where reconstructFasta [h, s] = Fasta (T.concat [">", h]) s 


outputFasta filename fa = do
  TextIO.writeFile (filename::FilePath) $ T.unlines $ map (\x->T.concat [faname x, "\n" , faseq x]) fa

importFastq filename = do
  TextIO.readFile filename >>= return . map (reconstructFastq . T.lines). tail . T.splitOn "@"
    where reconstructFastq [h, s, s2, q] = Fastq (T.concat ["@", h]) s s2 q

outputFastq filename fq = do
  TextIO.writeFile (filename::FilePath) $ T.unlines $ map fromFastq fq
    where fromFastq x = T.unlines [fqname x, fqseq x, fqstrand x, fqqual x]


