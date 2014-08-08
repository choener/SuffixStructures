
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

-- | Create ESA data structures from multiple sources and serialize to
-- disk.

module Main where

import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import qualified Data.Aeson as A
import           Data.Function (on)
import           Data.List (groupBy,sort)
import           Data.Tuple (swap)
import qualified Data.Binary as DB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Serialize as DS
import qualified Data.Vector.Unboxed as VU
import           System.Console.CmdArgs
import           Text.Printf

import           Data.SuffixStructure.ESA
import           Data.SuffixStructure.NaiveArray



data Type = Binary | Cereal | JSON
  deriving (Show,Data,Typeable)

data Options
  = RawBS
    { infile  :: String
    , outfile :: String
    , outtype :: Type
    }
  | ReadTest
    { infile  :: String
    , outfile :: String
    , intype  :: Type
    }
  deriving (Show,Data,Typeable)

-- | Read a file as a simple raw bytestring and create the enhanced suffix
-- array. Note: the input is a strict bytestring. We need to load the
-- complete bytestring anyway, so we make it strict. We don't mmap because
-- the whole string is loaded.

oRawBS = RawBS
  { infile  = def &= help ""
  , outfile = def &= help ""
  , outtype = Cereal &= help ""
  }

-- | Will read in the enhanced suffix array, print some statistics, and
-- quit.
--
-- TODO should actually become @Statistics@ I think.

oReadTest = ReadTest
  { intype = Cereal &= help ""
  }

main = do
  o <- cmdArgs $ modes [oRawBS, oReadTest]
  case o of
    RawBS{..} -> do i <- if null infile then B.getContents else B.readFile infile
                    let !ar = genSA i
                    let writer = if null outfile then BL.putStr else BL.writeFile outfile
                    writer $ case outtype of
                        Binary -> DB.encode ar
                        Cereal -> DS.encodeLazy ar
                        JSON   -> A.encode ar
    ReadTest{..} -> do  i <- if null infile then BL.getContents else BL.readFile infile
                        let ar :: SA = case intype of
                              Binary -> DB.decode i
                              Cereal -> case DS.decodeLazy i of
                                          Right d   -> d
                                          Left  err -> error err
                              JSON   -> case A.decode i of
                                          Just ar   -> ar
                                          Nothing   -> error "not JSON input"
                        printf "Suffix array size: %d\n" . VU.length $ sa ar
                        let lcpdist = IM.fromListWith (+) $
                                        (VU.toList . VU.map (,1::Int) . VU.filter (>=0) . VU.map fromIntegral $ lcp ar) ++
                                        (map (,1) . IM.elems $ lcpLong ar)
                        printf "LCP array distribution\n"
                        {-
                        let tmax = maximum $ IM.keys lcpdist
                        let ts = groupBy ((==) `on` (`div` 10)) [0 .. tmax]
                        forM_ ts $ \tt -> do
                          mapM_ (printf "%8d") $ tt
                          printf "\n"
                          mapM_ (printf "%8d") $ map (maybe 0 id . flip IM.lookup lcpdist) $ tt
                          printf "\n\n"
                        -}
                        let hs = take 14 . reverse . sort . map swap $ IM.assocs lcpdist
                        mapM_ (printf "%8d") $ map snd hs
                        printf "\n"
                        mapM_ (printf "%8d") $ map fst hs
                        printf "\n\n"

