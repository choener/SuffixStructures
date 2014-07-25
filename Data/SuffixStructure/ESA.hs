
module Data.SuffixStructure.ESA where

import Data.Vector.Unboxed (Vector)
import Data.Int (Int8)
import Data.IntMap.Strict (IntMap)



-- | The Suffix Array data type, together with the longest common prefix
-- table.
--
-- TODO maybe parametrize on the Int type (Int,Int64,Int32,Word's)
--
-- TODO fast serialization to disk needed

data SA = SA
  { sa  :: !(Vector Int)      -- ^ the actual suffix array using 8byte Ints
  , lcp :: !(Vector Int8)     -- ^ 1byte longest common prefix vector, negative number indicates to look at lcpLong
  , lcpLong :: !(IntMap Int)  -- ^ lcp's that are unusual long, but this is sparse
  }
  deriving (Eq,Ord,Show)

