{-# LANGUAGE DoAndIfThenElse #-}
-- Calculate Dilutionn Factor for IonTorrent Libraries
-- Min Zhang
-- Martin Lab
-- 9-8-2014
-- version 0.1.0 (3-21-2015)
module Main
where

import qualified System.Environment as Sys
import qualified Safe as Safe

main = do
  instructions
  parameters <- interactive
  test parameters
  let result = process parameters -- process is pure function
  output result

instructions = putStrLn "df: Ion Torrent library dilution factor calculator\n\
                        \  usage: df [length(bp)] [concentration(ng/ul)] [machine(Proton/Pgm)]\n"

interactive = do
  args <- Sys.getArgs
  if length args /= 3
  then return (Nothing, Nothing, Nothing)
  else
    let len = Safe.readMay (head args) :: Maybe Double
        conc = Safe.readMay (head $ drop 1 args) :: Maybe Double
        machine = Safe.readMay (head $ drop 2 args) ::Maybe Machine
    in return (len, conc, machine)

test (len, conc, machine) 
  | and [len == Nothing, conc == Nothing, machine == Nothing] = putStrLn "Not correct input format\n"
  | len == Nothing = putStrLn "Error: Length is not the good format.\n"
  | conc == Nothing = putStrLn "Error: Concentration is not the good format.\n"
  | machine == Nothing = putStrLn "Error: Machine type should be either Proton or Pgm.\n"
  | otherwise = putStrLn "Everything looks good.\n"
  

process (len, conc, machine)
  | or [len == Nothing, conc == Nothing, machine == Nothing] = "Error in input."
  | machine == Just Proton 	= show $ molarity * 1000.0 / 11.0
  | machine == Just Pgm		= show $ molarity * 1000.0 / 20
    where molarity 	= ((conc' * 1e-9 / 1e-6) / molecularWeight) * 1e9 
          molecularWeight = len' * 607.4 + 157.9 
          conc' = (\(Just x)->x) conc
          len' = (\(Just x)->x) len
          

output x = putStrLn $ "Dilution factor: " ++ x

data Machine = Proton | Pgm
  deriving (Show, Read, Eq)

  
