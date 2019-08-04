{-# LANGUAGE TypeFamilies #-}
module Data.Syntax.Extra (packed') where

import Control.Lens.Iso (Iso', iso)
import Data.MonoTraversable (Element)
import Data.Sequences (IsSequence)
import Data.Syntax (packed)
import GHC.Exts (IsList, Item)
import qualified GHC.Exts as IL (fromList, toList)

packed' :: (IsSequence seq, IsList list, Element seq ~ Item list) => Iso' seq list
packed' = packed . listIsoIsList

listIsoIsList :: (IsList l, Item l ~ a) => Iso' [a] l
listIsoIsList = iso IL.fromList IL.toList
