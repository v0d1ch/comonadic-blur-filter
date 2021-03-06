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
  -- verbose: Store (\x -> f' $ Store f x) s

  duplicate :: Store s a -> Store s (Store s a)
  duplicate (Store f s) = Store (\x -> Store f x) s

data Coord = Coord Int Int deriving (Eq, Ord, Show)

data RGB = RGB Word Word Word deriving (Show, Eq)

type Image = Store Coord (Maybe RGB)
type X     = Int
type Y     = Int

