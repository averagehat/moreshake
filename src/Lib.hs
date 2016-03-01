{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where
--    ( someFunc
--    ) where
-- lasers
import Options.Generic

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad
import Control.Applicative
import Data.List (tails)

data Example = Example { foo :: Int, bar :: Double }
    deriving (Generic, Show)

instance ParseRecord Example

someFunc = do
    x <- getRecord "Test program"
    print (x :: Example)



-- To run:
-- stack setup
-- stack exec shell-exe
replace xs ys rep = if (prefix xs ys) then (rep ++ (drop (length xs) ys)) else (head ys) : replace xs (tail ys) rep
prefix sub sup = (length sub <= length sup) && (all id $ zipWith (==) sub sup)
safeTake :: Int -> [a] -> Maybe [a]
safeTake 0 _      = Just []
safeTake n []     = Nothing
safeTake n (x:xs) = liftA2 (:) (Just x) (safeTake (n - 1) xs)
subs sub sup = any id $ map (prefix sub) (tails sup)

--block = shakeArgs shakeOptions{shakeFiles="_build"} $ do 
--    want ["data/align/align.bam"] 
--
--    "data/align/align.bam" %> \out -> do -- align.bam is the target
--       fqs <- allFqs
--       let trimmed = map (<.> "cutadapt") fqs
--       need trimmed
--       --TODO: replace with mapper
--       cmd Shell "cat" trimmed ">" out 
--
--    "data/*.cutadapt" %> \out -> do 
--       sffs <- getDirectoryFiles "" ["data/*.sff"]
--       let fqs = [c -<.> "fastq" | c <- sffs]  -- -<.> replaces extension
--       need fqs
--       allFastqs <- getDirectoryFiles "" ["data/*.fastq"] 
--       mapPairedUnpaired pCutAdapt unpCutAdapt  allFastqs 
--
--    "data/*.fastq" %> \out -> do 
--         need [out -<.> "sff"] 
--         runPython "from Bio import SeqIO \n\
--         \SeqIO.convert('${src}', 'sff', '${out}', 'fastq')"
--
--unpCutAdapt fq = unit $ cmd "cutadapt" ["-a", "AACCGGTT", "-o", fq <.> "cutadapt" , fq ] 
--pCutAdapt fwd rev = unit $ cmd "cutadapt" ["-a", "ADAPTER_FWD", "-A", "ADAPTER_REV", "-o", outFwd, "-p", outRev,  fwd, rev]
--  where (outFwd, outRev) = (fwd <.> "cutdapt", rev <.> "cutadapt")
--
--groupFastqs fqs = (unpaired, fwd, rev) 
--  where
--    fwd = filter (subs "_R1_") fqs
--    rev = filter (subs "_R2_") fqs 
--    unpaired = [x | x <- fqs, not (x `elem` (fwd ++ rev))]
--
---- FilePath is an alias for string
--mapPairedUnpaired :: (FilePath -> FilePath -> Action ()) -> (FilePath -> Action ()) -> [FilePath] -> Action ()
--mapPairedUnpaired pF unpF fqs = do
--  _ <- (mapM_ unpF unp) 
--  zipWithM_ pF fwd rev 
--  where (unp, fwd, rev) =  groupFastqs fqs
--
-- 
--allFqs = liftA2 (++) sffs fqs
--  where 
--    sffs = liftM (map (-<.> "fastq")) $ getDirectoryFiles "" ["data/*.sff"]
--    fqs =  getDirectoryFiles "" ["data/*.fastq"] 
--
--runPython str = do 
--  withTempFile $ \file -> do 
--    liftIO $ writeFile file fixedStr
--    cmd "python" file 
--  where 
--    isWhitespace = null . dropWhile (\x -> (x==' ') || (x == '\n'))
--    full = dropWhile isWhitespace $ lines str 
--    indent = length $ takeWhile (== ' ') $ head full
--    fixedStr = unlines $ map (drop indent) full
