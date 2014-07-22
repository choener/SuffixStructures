
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.DeepSeq
import           Criterion.Main
import           Data.Char (ord)
import           Data.Vector.Unboxed (Vector(..))
import qualified Data.Vector.Unboxed as VU
import           System.Console.CmdArgs
import           System.Random.MWC (withSystemRandom, asGenST, uniformVector)

import Data.SuffixStructure.NaiveArray



data Options = Options
  { from  :: Int
  , step  :: Int
  , count :: Int
  }
  deriving (Show,Data,Typeable)

options = Options
  { from  =  1
  , step  =  2
  , count = 20
  }

main = do
  Options{..} <- cmdArgs options
  rs :: [Vector Int] <- sequence . map (\k -> withSystemRandom . asGenST $ \gen -> uniformVector gen k) . take count $ (iterate (*step) from)
  deepseq rs $ defaultMain
    [ bgroup "naive"
        $ zipWith (\k r -> bench (show k) $ whnf (VU.length . sa . genSAvu) r)
                  (iterate (*step) from)
                  rs
    ]

