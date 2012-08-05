module Misc (
		combs,
		mapi,
		cached
	) where

import Data.Maybe
import qualified Data.Map as M

combs' :: a -> [a] -> [(a,a)]
combs' _ [] = []
combs' x (y:yr) = (x,y) : combs' x yr

combs :: [a] -> [a] -> [(a,a)]
combs [] y = []
combs (x:xr) y = combs' x y ++ combs xr y

mapi' :: (a -> Int -> b) -> [a] -> Int -> [b]
mapi' _ [] _ = []
mapi' f (x:r) i = f x i : mapi' f r (i+1)

mapi :: (a -> Int -> b) -> [a] -> [b]
mapi f l = mapi' f l 1

cached :: Ord a => (a -> b) -> a -> M.Map a b -> (b, M.Map a b)
cached f x l = if isJust r
	then (fromJust r, l)
	else (z, M.insert x z l)
		where
			r = M.lookup x l
			z = f x