
module Data.SuffixStructure.NaiveArray where

import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed (Vector(..))
import qualified Data.ByteString as B
import           Data.ByteString (ByteString(..))
import qualified Data.Vector.Algorithms.Intro as AI



data SA = SA
  { sa :: !(Vector Int)
  }
  deriving (Eq,Ord,Show)

genSA :: ByteString -> SA
genSA xs = SA . VU.modify (AI.sortBy srt) $ VU.enumFromN 0 (B.length xs)
  where srt i j = B.drop i xs `compare` B.drop j xs
{-# INLINE genSA #-}

genSAvu :: (VU.Unbox a, Ord a) => Vector a -> SA
genSAvu xs = SA . VU.modify (AI.sortBy srt) $ VU.enumFromN 0 (VU.length xs)
  where srt i j = VU.unsafeDrop i xs `compare` VU.unsafeDrop j xs
{-# INLINE genSAvu #-}

