{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
module Data.Syntax.Byte (SyntaxByte(..), texted) where

import Control.Category ((>>>))
import Control.Lens.Iso (Iso', iso)
import Control.Lens.Prism (Prism', prism')
import Control.SIArrow (SIArrow, (^<<), (>>^), sisequence_)
import Data.Bytes (ByteSeqNum, fromByteSeq, toByteSeq)
import qualified Data.ByteString as BS
import Data.MonoTraversable (Element)
import Data.Syntax (Syntax, Seq, anyChar, char, packed, vecN)
import Data.Syntax.Combinator (vec)
import Data.Sequences (IsSequence)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.ByteOrder (ByteOrder(..))

class (SIArrow syn, Syntax syn, Element (Seq syn) ~ Word8) => SyntaxByte syn where
  -- Constant matchers
  word8 :: Word8 -> syn () ()
  word8 = char

  wordX :: ByteSeqNum a => ByteOrder -> a -> syn () ()
  wordX bo w = sisequence_ $ leBo bo $ word8 <$> toByteSeq w

  word16 :: ByteOrder -> Word16 -> syn () ()
  word16 = wordX

  word32 :: ByteOrder -> Word32 -> syn () ()
  word32 = wordX

  word64 :: ByteOrder -> Word64 -> syn () ()
  word64 = wordX

  wordSeq :: [Word8] -> syn () ()
  wordSeq ws = sisequence_ $ word8 <$> ws

  -- Number acquiring
  anyWord8 :: syn () Word8
  anyWord8 = anyChar

  anyWordX :: ByteSeqNum a => Int -> ByteOrder -> syn () a
  anyWordX bytesCount bo = leBytesPrism ^<< leIsoBo bo ^<< packed ^<< vecN bytesCount anyWord8

  anyWord16 :: ByteOrder -> syn () Word16
  anyWord16 = anyWordX 2

  anyWord32 :: ByteOrder -> syn () Word32
  anyWord32 = anyWordX 4

  anyWord64 :: ByteOrder -> syn () Word32
  anyWord64 = anyWordX 8

  -- Sequence getting
  sizedByteSeq :: ByteSeqNum a => syn () a -> syn () (Vector Word8)
  sizedByteSeq size = (size >>^ byteIsoInt) >>> vec anyWord8

  -- Text function
  utf8Text :: Text -> syn () ()
  utf8Text = wordSeq . BS.unpack . encodeUtf8

instance (SIArrow syn, Syntax syn, Element (Seq syn) ~ Word8) => SyntaxByte syn

texted :: (IsSequence seq, Element seq ~ Word8) => Iso' seq Text
texted = packed . wordListIsoText

wordListIsoText :: Iso' [Word8] Text
wordListIsoText = iso (decodeUtf8 . BS.pack) (BS.unpack . encodeUtf8)

byteIsoInt :: ByteSeqNum a => Iso' a Int -- actually, this implementation can lose or corrupt runtime values
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
