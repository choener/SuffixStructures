
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
-- TODO need to check performance of 'drop' vs 'unsafeDrop'
--
-- TODO use 'Word' instead of 'Int' -- but check performance due to all
-- those conversions

module Data.SuffixStructure.NaiveArray where

import           Data.ByteString (ByteString(..))
import           Data.Int
import           Data.IntMap.Strict (IntMap(..))
import           Data.ListLike (ListLike)
import           Data.Vector.Unboxed (Vector(..))
import           Data.Word
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IM
import qualified Data.ListLike as LL
import qualified Data.Vector.Algorithms.AmericanFlag as AA
import qualified Data.Vector.Algorithms.Intro as AI
import qualified Data.Vector.Unboxed as VU

import           Data.SuffixStructure.ESA



-- | Create Suffix Array via Introsort

genSA :: (ListLike ll a, Ord ll, Eq a) => ll -> SA
genSA ll = SA sa lcp lcpLong where
  sa      = VU.modify (AI.sortBy srt) $ VU.enumFromN 0 (LL.length ll)
  (lcp,lcpLong) = buildLCP ll sa
  srt i j = LL.drop i ll `compare` LL.drop j ll
{-# INLINE genSA #-}

-- | Create Suffix Array via American Flag sort

genSAaf :: (ListLike ll a, Ord ll, AA.Lexicographic ll, Eq a) => ll -> SA
genSAaf ll = SA sa lcp lcpLong where
  sa       = VU.modify (AA.sortBy srt strp bckt rdx) $ VU.enumFromN 0 (LL.length ll)
  (lcp,lcpLong) = buildLCP ll sa
  srt i j  = LL.drop i ll `compare` LL.drop j ll
  strp _ i = i >= LL.length ll
  bckt     = AA.size ll
  rdx i _  = AA.index i ll
{-# INLINE genSAaf #-}

-- | Build LCP array

buildLCP :: (ListLike ll a, Eq a) => ll -> VU.Vector Int -> (VU.Vector Int8, IM.IntMap Int)
buildLCP inp sa = (lcp,lcpLong) where
  lcp = (-1) `VU.cons` VU.zipWith golcp sa (VU.tail sa)
  lcpLong = VU.foldl' golcpLong IM.empty $ VU.zip4 (VU.enumFromN 1 $ VU.length sa) sa (VU.tail sa) (VU.tail lcp)
  golcp p k = let cpl = commonPrefixLength (LL.drop p inp) (LL.drop k inp)
              in  if   cpl <= 127
                  then fromIntegral cpl
                  else (-2)
  golcpLong im (k,s,t,l)
    | l== -2    = IM.insert k (commonPrefixLength (LL.drop s inp) (LL.drop t inp)) im
    | otherwise = im
{-# INLINE buildLCP #-}

-- | Return the shared prefix of two strings.

commonPrefix :: (ListLike ll a, Eq a) => ll -> ll -> ll
commonPrefix xs ys = LL.take k xs where
  k = commonPrefixLength xs ys
{-# INLINE commonPrefixLength #-}

-- | Return the length of the common prefix.

commonPrefixLength :: (ListLike ll a, Eq a) => ll -> ll -> Int
commonPrefixLength = go 0 where
  go !k !x !y
    | LL.null x || LL.null y = k
    | LL.head x == LL.head y = go (k+1) (LL.tail x) (LL.tail y)
    | otherwise              = k
{-# INLINE commonPrefix #-}

{-
genSA :: ByteString -> SA
genSA xs = SA . VU.modify (AI.sortBy srt) $ VU.enumFromN 0 (B.length xs)
  where srt i j = B.drop i xs `compare` B.drop j xs
{-# INLINE genSA #-}

genSAvu :: (VU.Unbox a, Ord a) => Vector a -> SA
genSAvu xs = SA . VU.modify (AI.sortBy srt) $ VU.enumFromN 0 (VU.length xs)
  where srt i j = VU.unsafeDrop i xs `compare` VU.unsafeDrop j xs
{-# INLINE genSAvu #-}
-}

