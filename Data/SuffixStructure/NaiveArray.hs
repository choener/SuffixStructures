
-- |
--
-- TODO need to check performance of 'drop' vs 'unsafeDrop'
--
-- TODO use 'Word' instead of 'Int' -- but check performance due to all
-- those conversions

module Data.SuffixStructure.NaiveArray where

import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed (Vector(..))
import qualified Data.ByteString as B
import           Data.ByteString (ByteString(..))
import qualified Data.Vector.Algorithms.Intro as AI
import qualified Data.Vector.Algorithms.AmericanFlag as AA
import           Data.Int
import           Data.Word
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap(..))
import qualified Data.ListLike as LL
import           Data.ListLike (ListLike)



-- | The Suffix Array data type, together with the longest common prefix
-- table.

data SA = SA
  { sa  :: !(Vector Int)      -- ^ the actual suffix array using 8byte Ints
  , lcp :: !(Vector Int8)     -- ^ 1byte longest common prefix vector, negative number indicates to look at lcpLong
  , lcpLong :: !(IntMap Int)  -- ^ lcp's that are unusual long, but this is sparse
  }
  deriving (Eq,Ord,Show)

-- | Create Suffix Array via Introsort

genSA :: (ListLike ll a, Ord ll) => ll -> SA
genSA ll = SA sa lcp lcpLong where
  sa      = VU.modify (AI.sortBy srt) $ VU.enumFromN 0 (LL.length ll)
  lcp     = VU.empty
  lcpLong = IM.empty
  srt i j = LL.drop i ll `compare` LL.drop j ll

-- | Create Suffix Array via American Flag sort

genSAaf :: (ListLike ll a, Ord ll, AA.Lexicographic ll) => ll -> SA
genSAaf ll = SA sa lcp lcpLong where
  sa       = VU.modify (AA.sortBy srt strp bckt rdx) $ VU.enumFromN 0 (LL.length ll)
  lcp      = VU.empty
  lcpLong  = IM.empty
  srt i j  = LL.drop i ll `compare` LL.drop j ll
  strp _ i = i >= LL.length ll
  bckt     = AA.size ll
  rdx i _  = AA.index i ll

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

