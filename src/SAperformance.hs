
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.DeepSeq
import           Criterion.Main
import           Data.Char (ord)
import           Data.Vector.Unboxed (Vector(..))
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
  rs :: [Vector Int] <- sequence . map (\k -> withSystemRandom . asGenST $ \gen -> uniformVector gen k) . take count $ (iterate (*step) from)
  withArgs remaining . deepseq rs $ defaultMain
    [ bgroup "naive/genSAdef"
        $ zipWith (\k r -> bench (show k) $ whnf (VU.length . sa . genSA) r)
                  (iterate (*step) from)
                  rs
    , bgroup "naive/genSAaa"
        $ zipWith (\k r -> bench (show k) $ whnf (VU.length . sa . genSA) r)
                  (iterate (*step) from)
                  rs
    ]

