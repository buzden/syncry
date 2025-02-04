{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module SshSpec where

import Control.Category.Structures ((/+/))
import Control.Lens.TH (makePrisms)
import Control.SIArrow ((^<<), (/$/), (*/))
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.MonoTraversable (Element)
import qualified Data.Syntax.Attoparsec.ByteString.Lazy as S
import qualified Data.Syntax.Printer.ByteString.Lazy as P
import Data.Syntax.Byte (SyntaxByte, anyWord8, anyWord32, sizedByteSeq', texted, utf8Text, word8)
import Data.Syntax.Combinator (manyTill)
import Data.Syntax.Poly (takeWhile')
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.ByteOrder (ByteOrder(..))
import qualified GHC.Exts as IL (fromList)
import Test.Hspec (describe, it, shouldBe)
import Test.HUnit.Lang (assertFailure)

ascii = IL.fromList . BL.unpack . BC.pack

shouldParseAs spec a b =
   case AP.parse (S.getParser_ spec <* AP.endOfInput) a of
        AP.Done _ pl -> pl `shouldBe` b
        x -> assertFailure $ "parsing not done: " <> show x

shouldGenerateAs spec a b =
   (B.toLazyByteString <$> P.runPrinter_ spec a) `shouldBe` Right b

data Payload = Version Text
             | Ignore (Vector Word8)
             | ServiceRequest (Vector Word8)
             | ServiceAccept (Vector Word8)
             deriving (Show, Eq)

$(makePrisms ''Payload)

ssh2payload :: (SyntaxByte syn) => syn () Payload
ssh2payload = version /+/ ignore /+/ servReq
   -- BUG: version ... vecN 5 anyWord8 -- reverse order
   where version = _Version /$/ utf8Text "SSH-2.0-" */ (texted ^<< anyWord8 `manyTill` word8 10)
         ignore = _Ignore /$/ word8 2 */ takeWhile' (const True)
         servReq = _ServiceRequest /$/ word8 5 */ sizedByteSeq' (anyWord32 LittleEndian)

spec = describe "SSH spec" do
   it "parses" do
      let shouldParsePayload = shouldParseAs ssh2payload
      "SSH-2.0-TesT\r\n" `shouldParsePayload` Version "TesT\r"
      "\x2__" `shouldParsePayload` Ignore (ascii "__")
      "\x5\x6\0\0\0tested" `shouldParsePayload` ServiceRequest (ascii "tested")

   it "generates" do
      let shouldGeneratePayload = shouldGenerateAs ssh2payload
      Version "TesT\r" `shouldGeneratePayload` "SSH-2.0-TesT\r\n"
