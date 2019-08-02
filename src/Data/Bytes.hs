{-# LANGUAGE DefaultSignatures #-}
module Data.Bytes (ByteSeqNum(..)) where

import Data.Bits (FiniteBits, (.|.), finiteBitSize, shiftL, shiftR, zeroBits)
import Data.List (unfoldr)
import Data.Word (Word8, Word16, Word32, Word64)

class (FiniteBits b, Integral b) => ByteSeqNum b where
  -- Output sequence is little endian, i.e. LSB is the first.
  toByteSeq :: b -> [Word8]
  toByteSeq 0 = [0]
  toByteSeq x = unfoldr cutByte x
    where
      cutByte 0 = Nothing
      cutByte b = Just (lowestByte b, shiftR b 8)
        where
          lowestByte x = fromInteger $ toInteger x `mod` 2^8

  fromByteSeq :: [Word8] -> Maybe b

-- Input sequence is expected to be little endian, i.e. LSB is the first.
-- Nothing is returned is the given sequence is too long.
defaultFromByteSeq :: ByteSeqNum b => Int -> [Word8] -> Maybe b
defaultFromByteSeq maxBytes bs | length bs > maxBytes = Nothing
                               | otherwise = Just $ foldr shiftNAdd 0 bs
  where
    shiftNAdd w8 currB = shiftL currB 8 .|. byteAsB w8
    byteAsB = fromInteger . toInteger -- assuming that b is not less than Word8 since `FiniteBytes b`

instance ByteSeqNum Word8 where
  fromByteSeq = defaultFromByteSeq 1

instance ByteSeqNum Word16 where
  fromByteSeq = defaultFromByteSeq 2

instance ByteSeqNum Word32 where
  fromByteSeq = defaultFromByteSeq 4

instance ByteSeqNum Word64 where
  fromByteSeq = defaultFromByteSeq 8
