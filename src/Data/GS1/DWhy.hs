{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhy where

import           Control.Lens
import           GHC.Generics
import           Data.GS1.EPC
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Text
import           Data.Swagger
import           Database.SQLite.Simple.ToField
import qualified Data.Text.Lazy as TxtL

--        should this be BizStep now? - @sa
data DWhy = DWhy (Maybe BizStep) (Maybe Disposition)
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''DWhy)
instance ToSchema DWhy

instance ToField DWhy where
  toField = toField . TxtL.toStrict . encodeToLazyText
{-
XXX - FIXME
this is not a lawful lens
it is a Traversal though
you might also want makeClassyPrisms for BizStep (as well as, or instead of, makeClassy)
-}
instance HasBizStep DWhy where
  bizStep =
    lens
    (\(DWhy (Just b) _) -> b)
    (\(DWhy _ d) b -> DWhy (Just b) d)

-- FIXME The usage of lens should be reconsidered
-- Nothing is the cause
makeClassy ''DWhy

-- XXX mkDWhy can be rewritten as:
--mkDWhy = liftA2 DWhy
mkDWhy :: Either ParseFailure BizStep -> Either ParseFailure Disposition -> Maybe DWhy
mkDWhy (Right step) (Right disp) = Just $ DWhy (Just step) (Just disp)
mkDWhy _ _                       = Nothing
