module Data.GS1.EPC where

import Data.GS1.Location

-- |Elctronic Product Code
-- It could represented by many standards
-- For example GLN (GTIN13) is one of them
-- Currently it has one method to convert the meaningful EPC to a consecutive string
class EPC a where
  epc :: a -> String

instance EPC GLN where
  epc (GLN pref ref cd) = concat [pref, ref, cd]
