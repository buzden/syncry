module Data.Syntax.Byte (SyntaxByte) where

import Data.Bits (FiniteBits, shiftL, shiftR, zeroBits)
import Data.Bytes (FiniteBytes)
import Data.Vector.Unboxed (Vector, fromList)
import Data.Word (Word8)
import GHC.ByteOrder (ByteOrder(..))

class (Syntax syn, Element (Seq syn) ~ Word8) => SyntaxByte syn where
  bytes :: FiniteBytes a => ByteOrder -> a -> syn () ()

  anyBytes :: FiniteBytes a => ByteOrder -> syn () a
  anyBytes = vecN finiteBytesSize anyChar >>^ vecBytesIsoNum

vecBytesIsoNum :: SemiIso' (Vector Word8) a
vecBytesIsoNum = semiIso (pure . foldLittleEndianBitsSeq) (pure . fromList . unfoldLittleEndianBitsSeq)

foldLittleEndianBitsSeq :: (Foldable t, Integral a, FiniteBits b, Num b) => t a -> b
foldLittleEndianBitsSeq = foldr f 0
  where
    f a curr = shifted curr .|. coerseAsBits a
    shifted b = shiftL b $ finiteBitsSize b

unfoldLittleEndianBitsSeq :: (Integral b, FiniteBits a, Num a) => b -> [a]
unfoldLittleEndianBitsSeq = unfoldr f
  where
    f b = if b == zeroBits
      then Nothing
      else Just (coerceAsBits b, shiftR b $ finiteBitsSize b)

coerceAsBits :: (Integral a, FiniteBits b, Num b) => a -> b
coerceAsBits = fromInteger . (.&. maskForB) . toInteger
  where
    maskForB = foldr (.|.) 0 $ fmap bit $ [0 .. (finiteBitsSize zeroBits::b) - 1]
