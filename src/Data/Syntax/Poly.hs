{-# LANGUAGE TypeFamilies #-}
module Data.Syntax.Poly (
    packed',
    manyTill',
    sepBy', sepBy1',
    take', takeArr',
    takeWhile', takeWhile1',
    takeTill', takeTill1',
    vecN', vecNSepBy',
    vec', vecSepBy',
    ) where

import Prelude hiding (take, takeWhile)

import Control.Lens.Iso (Iso', iso)
import Control.SIArrow ((/*), (>>^))
import Data.MonoTraversable (Element)
import Data.Sequences (IsSequence)
import Data.Syntax (Syntax, Seq, char, packed, take, takeTill, takeTill1, takeWhile, takeWhile1, vecN)
import Data.Syntax.Combinator (manyTill, sepBy, sepBy1, takeArr, vec, vecNSepBy, vecSepBy)
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IL (fromList, toList)

packed' :: (IsSequence seq, IsList list, Element seq ~ Item list) => Iso' seq list
packed' = packed . listIsoIsList

listIsoIsList :: (IsList l, Item l ~ a) => Iso' [a] l
listIsoIsList = iso IL.fromList IL.toList

manyTill' :: (Syntax syn, IsList l, Item l ~ a) => syn () a -> syn () () -> syn () l
manyTill' a term = manyTill a term >>^ packed'

sepBy' :: (Syntax syn, IsList l, Item l ~ a) => syn () a -> syn () () -> syn () l
sepBy' a sep = sepBy a sep >>^ packed'

sepBy1' :: (Syntax syn, IsList l, Item l ~ a) => syn () a -> syn () () -> syn () l
sepBy1' a sep = sepBy1 a sep >>^ packed'

take' :: (Syntax syn, IsList l, Item l ~ Element (Seq syn)) => Int -> syn () l
take' n = take n >>^ packed'

takeArr' :: (Syntax syn, IsList l, Item l ~ Element (Seq syn)) => syn Int l
takeArr' = takeArr >>^ packed'

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

vec' :: (Syntax syn, IsList l, Item l ~ a) => syn () a -> syn Int l
vec' a = vec a >>^ packed'

vecSepBy' :: (Syntax syn, IsList l, Item l ~ a) => syn () a -> syn () () -> syn Int l
vecSepBy' a sep = vecSepBy a sep >>^ packed'

vecNSepBy' :: (Syntax syn, IsList l, Item l ~ a) => Int -> syn () a -> syn () () -> syn () l
vecNSepBy' n a sep = vecNSepBy n a sep >>^ packed'
