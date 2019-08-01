{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Data.Syntax.Byte (SyntaxByte) where

import Control.Lens.SemiIso (SemiIso', semiIso)
import Control.SIArrow
import Data.Bits (FiniteBits, (.|.), shiftL, shiftR, zeroBits)
import Data.Bytes (FiniteBytes)
import Data.List as L (unfoldr)
import Data.MonoTraversable (Element)
import Data.Syntax
import Data.Vector (Vector, fromList)
import qualified Data.Vector as V (reverse)
import Data.Word (Word8, Word16)
import GHC.ByteOrder (ByteOrder(..))

class (Syntax syn, Element (Seq syn) ~ Word8) => SyntaxByte syn where
  word8 :: Word8 -> syn () ()
  word8 = char

  anyWord8 :: syn () Word8
  anyWord8 = anyChar

  word16 :: ByteOrder -> Word16 -> syn () ()

  anyWord16 :: ByteOrder -> syn () Word16
  anyWord16 bo = vecN 2 anyWord8 >>^ vecBytesIsoNum bo

vecBytesIsoNum :: (FiniteBytes a, Integral a) => ByteOrder -> SemiIso' (Vector Word8) a
vecBytesIsoNum bo = semiIso (pure . foldLittleEndianBytes . rev bo) (pure . rev bo . fromList . unfoldLittleEndianBytes)
  where
    rev LittleEndian = id
    rev BigEndian = V.reverse

foldLittleEndianBytes :: (Foldable t, FiniteBytes b, Num b) => t Word8 -> b
foldLittleEndianBytes = foldr f 0
  where
    f w8 currB = shiftL currB 8 .|. byteAsB w8
    byteAsB    = fromInteger . toInteger -- assuming that b is not less than Word8 since `FiniteBytes b`

unfoldLittleEndianBytes :: (Integral b, FiniteBytes b) => b -> [Word8]
unfoldLittleEndianBytes = L.unfoldr f
  where
    f b = if b == zeroBits
      then Nothing
      else Just (lowestByte b, shiftR b 8)
    lowestByte x = fromInteger $ toInteger x `mod` 2^8
