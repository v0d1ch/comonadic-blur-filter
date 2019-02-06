{-# LANGUAGE InstanceSigs #-}

module Types where

import Control.Comonad
import Data.Word (Word8 (..))

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

data RGB = RGB Word8 Word8 Word8 deriving Show

type Image = Store Coord RGB
