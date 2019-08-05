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
import Data.Syntax.Extra
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.Syntax.Attoparsec.ByteString.Lazy as S
import qualified Data.ByteString.Builder as B
import qualified Data.Syntax.Printer.ByteString.Lazy as P
import Test.HUnit.Lang (assertFailure)
import Data.Text (Text)

asciiL = BL.unpack . BC.pack
ascii = fromList . BL.unpack . BC.pack

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
   -- BUG: version ... takeTill -- "not enough input"
   -- BUG: version ... vecN 5 anyWord8 -- reverse order
   where version = _Version /$/ utf8Text "SSH-2.0-" */ (texted ^<< endingWith 10)
         ignore = _Ignore /$/ word8 2 */ (packed' ^<< takeWhile (const True))
         servReq = _ServiceRequest /$/ word8 5 */ sizedByteSeq (anyWord32 LittleEndian)

spec = describe "SSH spec" do
   it "parses" do
      let shouldParsePayload = shouldParseAs ssh2payload
      "SSH-2.0-TesT\r\n" `shouldParsePayload` Version "TesT\r"
      "\x2__" `shouldParsePayload` Ignore (ascii "__")
      "\x5\x6\0\0\0test" `shouldParsePayload` ServiceRequest (ascii "test")

   it "generates" do
      let shouldGeneratePayload = shouldGenerateAs ssh2payload
      Version "TesT\r" `shouldGeneratePayload` "SSH-2.0-TesT\r\n"
