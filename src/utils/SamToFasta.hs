{-#LANGUAGE OverloadedStrings#-}
-- convert sam file to fastq file
-- Min Zhang
-- 10-10-14
-- Version 0.0.1

import qualified System.Environment as Sys
import IO
import qualified Safe

main = do
  instructions
  inputArguments <- interactive
  test inputArguments
  process inputArguments

instructions = putStrLn "\nsamToFasta: Convert unmapped sam files to fasta files.\n\
                        \ usage: ./samToFastq [input sam file path] [output fasta file path] \n"

interactive = do  
  paths <- Sys.getArgs
  let input = Just (head paths)
  let output = Just (last paths)
  if length paths /= 2
    then do return (Nothing, Nothing)
    else do return (input, output)

test (input, output)
  | and [input == Nothing, output == Nothing] = putStrLn "The input needs to be two files, input sam and output fasta. \n"
  | fmap (reverse . take 4) input /= Just "mas." = putStrLn "The input needs to be Sam file. \n"
  | fmap (reverse . take 3) output /= Just "af." = putStrLn "The output needs to be Fasta file. \n"
  | otherwise = putStrLn "Everything looks good. \n"

process (input, output)
  | (input, output) == (Nothing, Nothing) = putStrLn "Error: please check your input.\n"
  | otherwise = samToFasta input' output'
      where input' = toPath input
            output' = toPath output
            toPath = \(Just x)->x::FilePath
   
