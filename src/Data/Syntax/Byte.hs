{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
module Data.Syntax.Byte (SyntaxByte(..)) where

import Control.Category ((>>>))
import Control.Lens.Iso (Iso', iso)
import Control.Lens.Prism (Prism', prism')
import Control.Lens.SemiIso (SemiIso', semiIso)
import Control.SIArrow
import Data.Bits (FiniteBits, (.|.), shiftL, shiftR, zeroBits)
import Data.Bytes (ByteSeqNum, fromByteSeq, toByteSeq)
import Data.List as L (unfoldr)
import Data.MonoTraversable (Element)
import Data.Syntax
import Data.Syntax.Combinator (vec)
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, reverse, toList)
import Data.Word (Word8, Word16, Word32)
import GHC.ByteOrder (ByteOrder(..))
import qualified Data.Syntax.Attoparsec.ByteString.Lazy as S

class (SIArrow syn, Syntax syn, Element (Seq syn) ~ Word8) => SyntaxByte syn where
  word8 :: Word8 -> syn () ()
  word8 = char

  anyWord8 :: syn () Word8
  anyWord8 = anyChar

  word16 :: ByteOrder -> Word16 -> syn () ()
  word16 bo w = sisequence_ $ leBo bo $ word8 <$> toByteSeq w

  anyWord16 :: ByteOrder -> syn () Word16
  anyWord16 bo = leBytesPrism ^<< leIsoBo bo ^<< packed ^<< vecN 2 anyWord8

  word32 :: ByteOrder -> Word32 -> syn () ()
  word32 bo w = sisequence_ $ leBo bo $ word8 <$> toByteSeq w

  anyWord32 :: ByteOrder -> syn () Word32
  anyWord32 bo = leBytesPrism ^<< leIsoBo bo ^<< packed ^<< vecN 4 anyWord8

  sizedByteSeq :: ByteSeqNum a => syn () a -> syn () (Vector Word8)
  sizedByteSeq size = (size >>^ byteIsoInt) >>> vec anyWord8

instance (SIArrow syn, Syntax syn, Element (Seq syn) ~ Word8) => SyntaxByte syn

byteIsoInt :: ByteSeqNum a => Iso' a Int -- actually, this implementation can loses or corrupt runtime values
byteIsoInt = iso (fromInteger . toInteger) (fromInteger . toInteger)

leBytesPrism :: ByteSeqNum a => Prism' [Word8] a
leBytesPrism = prism' toByteSeq fromByteSeq

-- semiiso between little-endian and lists of given endianness
leIsoBo :: ByteOrder -> Iso' [a] [a]
leIsoBo bo = iso l l
  where
    l = leBo bo

leBo :: ByteOrder -> [a] -> [a]
leBo LittleEndian = id
leBo BigEndian    = reverse
