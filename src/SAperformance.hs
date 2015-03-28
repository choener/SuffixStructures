
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.DeepSeq
import           Criterion.Main
import           Data.Char (ord,chr)
import           Data.Vector.Unboxed (Vector(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as VU
import           System.Console.CmdArgs
import           System.Environment (withArgs)
import           System.Random.MWC (withSystemRandom, asGenST, uniformVector)

import Data.SuffixStructure.ESA
import Data.SuffixStructure.NaiveArray



data Options = Options
  { from      :: Int
  , step      :: Int
  , count     :: Int
  , remaining :: [String]
  }
  deriving (Show,Data,Typeable)

options = Options
  { from      =   1 &= help ""
  , step      =  10 &= help ""
  , count     =   5 &= help ""
  , remaining = def &= args
  }

main = do
  o@Options{..} <- cmdArgs options
  is :: [Vector Int] <- sequence . map (\k -> withSystemRandom . asGenST $ \gen -> uniformVector gen k) . take count $ (iterate (*step) from)
  let cs :: [Vector Char] = map (VU.map (chr . flip mod 256)) is
  let bs :: [BS.ByteString] = map (BS.pack . VU.toList) cs
  deepseq (is,cs,bs) $ putStrLn "vectors generated"
  withArgs remaining $ defaultMain
    [ bgroup "naive/introsort"
        $ zipWith (\k r -> bench (show k) $ whnf (VU.length . sa . genSA) r)
                  (iterate (*step) from)
                  cs
    , bgroup "naive/americanflag"
        $ zipWith (\k r -> bench (show k) $ whnf (VU.length . sa . genSAaf) r)
                  (iterate (*step) from)
                  bs
    ]

