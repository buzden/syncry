{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module SshSpec where

import Prelude hiding (takeWhile)
import Test.Hspec
import Data.Word
import Control.SIArrow
import Control.Category.Structures
import Control.Lens.TH
import GHC.ByteOrder
import Data.Vector hiding (takeWhile)
import Data.MonoTraversable (Element)
import Data.Syntax
import Data.Syntax.Combinator
import Data.Syntax.Byte
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.Syntax.Attoparsec.ByteString.Lazy as S
import qualified Data.ByteString.Builder as B
import qualified Data.Syntax.Printer.ByteString.Lazy as P
import Test.HUnit.Lang (assertFailure)

ascii = fromList . BL.unpack . BC.pack

shouldParseAs spec a b =
   case AP.parse (S.getParser_ spec <* AP.endOfInput) a of
        AP.Done _ pl -> pl `shouldBe` b
        x -> assertFailure $ "parsing not done: " <> show x

shouldGenerateAs spec a b =
   (B.toLazyByteString <$> P.runPrinter_ spec a) `shouldBe` Right b

data Payload = Version (Vector Word8)
             | Ignore (Vector Word8)
             | ServiceRequest (Vector Word8)
             | ServiceAccept (Vector Word8)
             deriving (Show, Eq)

$(makePrisms ''Payload)

ssh2payload :: (SyntaxByte syn) => syn () Payload
ssh2payload = version /+/ ignore /+/ servReq
   where version = _Version /$/ word8 83 */ word8 83 */ word8 72 */ word8 45 */ word8 50 */ word8 46 */ word8 48 */ word8 45 */ vecN 4 anyWord8 -- takeWhile (const True)
         ignore = _Ignore /$/ word8 2 */ vecN 1 anyWord8 -- takeWhile (const True)
         servReq = _ServiceRequest /$/ word8 5 */ sizedByteSeq (anyWord32 LittleEndian)

spec = describe "SSH spec" do
   it "parses" do
      let shouldParsePayload = shouldParseAs ssh2payload
      "SSH-2.0-TesT" `shouldParsePayload` Version (ascii "TesT")
      "\x2_" `shouldParsePayload` Ignore (ascii "_")
      "\x5\x4\0\0\0test" `shouldParsePayload` ServiceRequest (ascii "test")

   it "generates" do
      let shouldGeneratePayload = shouldGenerateAs ssh2payload
      Version (ascii "TesT") `shouldGeneratePayload` "SSH-2.0-TesT"
