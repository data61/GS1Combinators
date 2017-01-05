{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}


module Data.GS1.DWhy where

import           Control.Lens
import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)

import           Data.GS1.BizStep
import           Data.GS1.BizTransaction
import           Data.GS1.Disposition
import           Data.GS1.SourceDest
import           Data.Maybe
import           GHC.Generics

data DWhy = DWhy
  {
    bizStep     :: Maybe BizStep
  , disposition :: Maybe Disposition
  }
  deriving (Show, Eq, Generic)

instance HasBizStep DWhy where
  bizStep =
    lens
    (\(DWhy (Just b) _) -> b)
    (\(DWhy _ d) b -> DWhy (Just b) d)

makeClassy ''DWhy

why2 :: (AsDispositionError e, MonadError e m)
     => Maybe BizStep -> Maybe Disposition -> m DWhy
why2 step disp
  | isNothing step || isNothing disp = pure (DWhy step disp)  -- TODO: verify when encounter Nothing
  | otherwise                        = if dispositionValidFor (fromJust step) (fromJust disp)
                                          then pure (DWhy step disp)
                                          else throwing _InvalidDisposition ()

data DWhyCBVCompliant = DWhyCBVCompliant BizStep (Maybe Disposition)
  deriving (Show, Eq, Generic)

-- TODO can we verify validity better when disposition is Nothing?
whyCBVCompliant :: (AsDispositionError e, MonadError e m)
     => BizStep -> Maybe Disposition -> m DWhyCBVCompliant
whyCBVCompliant step disp = case disp of
                              Nothing -> pure (DWhyCBVCompliant step Nothing)
                              Just d -> if dispositionValidFor step d
                                           then pure (DWhyCBVCompliant step (Just d))
                                           else throwing _InvalidDisposition ()

