{-# LANGUAGE InstanceSigs #-}

module Types where

import Control.Comonad

data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap :: (a -> b) -> Store s a -> Store s b
  fmap f (Store f' x) = Store (fmap f f') x

instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store f s) = f s

  extend :: (Store s a -> b) -> Store s a -> Store s b
  extend f = fmap f . duplicate

data Coord = Coord Int Int deriving (Eq, Ord, Show)

data RGB = RGB Int Int Int deriving Show

type Image = Store Coord (Maybe RGB)
type X     = Int
type Y     = Int
