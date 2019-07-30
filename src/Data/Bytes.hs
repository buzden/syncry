{-# LANGUAGE DefaultSignatures #-}
module Data.Bytes where

import Data.Bits (FiniteBits, finiteBitSize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

class FiniteBits b => FiniteBytes b where
  finiteByteSize :: b -> Int

  default finiteByteSize :: FiniteBits b => b -> Int
  finiteByteSize b = (finiteBitSize b) `quot` 8

instance FiniteBytes Int8
instance FiniteBytes Int16
instance FiniteBytes Int32
instance FiniteBytes Int64
instance FiniteBytes Word8
instance FiniteBytes Word16
instance FiniteBytes Word32
instance FiniteBytes Word64
