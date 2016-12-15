{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}


module Data.GS1.Why where

import           Control.Lens
import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)

import           Data.GS1.BizStep
import           Data.GS1.BizTransaction
import           Data.GS1.Disposition
import           Data.GS1.SourceDest
import           Data.Maybe
import           GHC.Generics

data Why = Why
  {
    bizStep     :: Maybe BizStep
  , disposition :: Maybe Disposition
  }
  deriving (Show, Eq, Generic)

instance HasBizStep Why where
  bizStep =
    lens
    (\(Why (Just b) _) -> b)
    (\(Why _ d) b -> Why (Just b) d)

makeClassy ''Why

why2 :: (AsDispositionError e, MonadError e m)
     => Maybe BizStep -> Maybe Disposition -> m Why
why2 step disp
  | isNothing step || isNothing disp = pure (Why step disp)  -- TODO: verify when encounter Nothing
  | otherwise                        = if dispositionValidFor (fromJust step) (fromJust disp)
                                          then pure (Why step disp)
                                          else throwing _InvalidDisposition ()

data WhyCBVCompliant = WhyCBVCompliant BizStep (Maybe Disposition)
  deriving (Show, Eq, Generic)

-- TODO can we verify validity better when disposition is Nothing?
whyCBVCompliant :: (AsDispositionError e, MonadError e m)
     => BizStep -> Maybe Disposition -> m WhyCBVCompliant
whyCBVCompliant step disp = case disp of
                              Nothing -> pure (WhyCBVCompliant step Nothing)
                              Just d -> if dispositionValidFor step d
                                           then pure (WhyCBVCompliant step (Just d))
                                           else throwing _InvalidDisposition ()


