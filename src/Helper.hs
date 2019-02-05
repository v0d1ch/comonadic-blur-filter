module Helper where

import Types

-- Get the store focus
pos :: Store s a -> s
pos (Store _ a) = a

-- Read out the store at some specific position
peek :: s -> Store s a -> a
peek s (Store f _) = f s

-- Modify the current focus, and read the store using the new focus.
peeks :: (s -> s) -> Store s a -> a
peeks f (Store f' s) = f' (f s)

-- Set the current focus
seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

-- Modify the current focus
seeks :: (s -> s) -> Store s a -> Store s a
seeks f (Store f' s) = Store f' (f s)

-- Run an experiment in the store.
experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment f (Store s a) = fmap s (f a)
