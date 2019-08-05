{-# LANGUAGE TypeFamilies #-}
module Data.Syntax.Extra (
    packed',
    take',
    takeWhile', takeWhile1',
    takeTill', takeTill1',
    vecN',
    endingWith) where

import Prelude hiding (take, takeWhile)

import Control.Lens.Iso (Iso', iso)
import Control.SIArrow ((/*), (>>^))
import Data.MonoTraversable (Element)
import Data.Sequences (IsSequence)
import Data.Syntax (Syntax, Seq, char, packed, take, takeTill, takeTill1, takeWhile, takeWhile1, vecN)
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IL (fromList, toList)

packed' :: (IsSequence seq, IsList list, Element seq ~ Item list) => Iso' seq list
packed' = packed . listIsoIsList

listIsoIsList :: (IsList l, Item l ~ a) => Iso' [a] l
listIsoIsList = iso IL.fromList IL.toList

endingWith :: Syntax syn => Element (Seq syn) -> syn () (Seq syn)
endingWith terminator = takeWhile (/= terminator) /* char terminator

take' :: (Syntax syn, IsList l, Item l ~ Element (Seq syn)) => Int -> syn () l
take' n = take n >>^ packed'

takeWhile' :: (Syntax syn, IsList l, Item l ~ Element (Seq syn)) => (Element (Seq syn) -> Bool) -> syn () l
takeWhile' c = takeWhile c >>^ packed'

takeWhile1' :: (Syntax syn, IsList l, Item l ~ Element (Seq syn)) => (Element (Seq syn) -> Bool) -> syn () l
takeWhile1' c = takeWhile1 c >>^ packed'

takeTill' :: (Syntax syn, IsList l, Item l ~ Element (Seq syn)) => (Element (Seq syn) -> Bool) -> syn () l
takeTill' c = takeTill c >>^ packed'

takeTill1' :: (Syntax syn, IsList l, Item l ~ Element (Seq syn)) => (Element (Seq syn) -> Bool) -> syn () l
takeTill1' c = takeTill1 c >>^ packed'

vecN' :: (Syntax syn, IsList l, Item l ~ a) => Int -> syn () a -> syn () l
vecN' n a = vecN n a >>^ packed'
