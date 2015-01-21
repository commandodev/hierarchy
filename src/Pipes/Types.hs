{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Pipes.Types where

import           Control.Foldl
import           Data.Profunctor


instance Profunctor Fold where
  dimap f g (Fold step begin done) = Fold step' begin (g . done)
    where
    step' x = step x . f
