{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Data.Syntax.Byte (SyntaxByte) where

import Control.Lens.Iso (Iso', iso)
import Control.Lens.Prism (Prism', prism')
import Control.Lens.SemiIso (SemiIso', semiIso)
import Control.SIArrow
import Data.Bits (FiniteBits, (.|.), shiftL, shiftR, zeroBits)
import Data.Bytes (ByteSeqNum, fromByteSeq, toByteSeq)
import Data.List as L (unfoldr)
import Data.MonoTraversable (Element)
import Data.Syntax
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, reverse, toList)
import Data.Word (Word8, Word16)
import GHC.ByteOrder (ByteOrder(..))

class (Syntax syn, Element (Seq syn) ~ Word8) => SyntaxByte syn where
  word8 :: Word8 -> syn () ()
  word8 = char

  anyWord8 :: syn () Word8
  anyWord8 = anyChar

  word16 :: ByteOrder -> Word16 -> syn () ()

  anyWord16 :: ByteOrder -> syn () Word16
  anyWord16 bo = leBytesPrism ^<< leIsoBo bo ^<< vecIsoList ^<< vecN 2 anyWord8

leBytesPrism :: ByteSeqNum a => Prism' [Word8] a
leBytesPrism = prism' toByteSeq fromByteSeq

vecIsoList :: Iso' (Vector a) [a]
vecIsoList = iso V.toList V.fromList

-- semiiso between little-endian and lists of given endianness
leIsoBo :: ByteOrder -> Iso' [a] [a]
leIsoBo LittleEndian = id
leIsoBo BigEndian    = iso reverse reverse
