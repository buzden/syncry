{-# LANGUAGE TypeFamilies #-}
module Data.Syntax.Extra (packed', endingWith) where

import Prelude hiding (takeWhile)

import Control.Lens.Iso (Iso', iso)
import Control.SIArrow ((/*))
import Data.MonoTraversable (Element)
import Data.Sequences (IsSequence)
import Data.Syntax (Syntax, Seq, char, packed, takeWhile)
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IL (fromList, toList)

packed' :: (IsSequence seq, IsList list, Element seq ~ Item list) => Iso' seq list
packed' = packed . listIsoIsList

listIsoIsList :: (IsList l, Item l ~ a) => Iso' [a] l
listIsoIsList = iso IL.fromList IL.toList

endingWith :: Syntax syn => Element (Seq syn) -> syn () (Seq syn)
endingWith terminator = takeWhile (/= terminator) /* char terminator
