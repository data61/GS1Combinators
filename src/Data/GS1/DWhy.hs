{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhy where

import           Control.Lens
import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)

import           Data.GS1.BizStep
import           Data.GS1.Disposition
import           Data.Maybe
import           GHC.Generics

data DWhy = DWhy (Maybe BizStep) (Maybe Disposition)
  deriving (Show, Eq, Generic)

instance HasBizStep DWhy where
  bizStep =
    lens
    (\(DWhy (Just b) _) -> b)
    (\(DWhy _ d) b -> DWhy (Just b) d)

-- FIXME The usage of lens should be reconsidered
-- Nothing is the cause
makeClassy ''DWhy

-- TODO: Remove the following function with dispositionValidFor
-- It does not work even with the sample
mkDWhy' :: (AsDispositionError e, MonadError e m)
     => Maybe BizStep -> Maybe Disposition -> m DWhy
mkDWhy' step disp
  | isNothing step || isNothing disp = pure (DWhy step disp)
  | otherwise                        = if dispositionValidFor (fromJust step) (fromJust disp)
                                          then pure (DWhy step disp)
                                          else throwing _InvalidDisposition ()

mkDWhy :: Maybe BizStep -> Maybe Disposition -> Maybe DWhy
mkDWhy step disp = if isNothing step || isNothing disp then Nothing
                     else Just $ DWhy step disp
