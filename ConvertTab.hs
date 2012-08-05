-- ---------------------------------------------------------------------------
-- | This Module is responsible for taking a Tab (as defined and generated 
-- from DecodeTab) and converting it into a tab playable on a ukulele. It does
-- this by generating all possible methods of playing the given notes or chord
-- on a ukulele, and then rating how easy they are to play and how easy it is
-- to move from one to another. Finally it picks the best path along every note
-- and returns it.

module ConvertTab (
		buildWeightedMap,
		convertTab,
		optPath, -- 
		outMoment, --
		OutTab 
	) where

import System.Environment
import System.FilePath
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char
import qualified Data.Map as M

import DecodeTab
import GeneratePossiblities
import Dijkstra
import Misc

type OutTab = [(Maybe String, Maybe Int, Maybe Int, Maybe Int, Maybe Int)]

--buildWeightedMap :: PossMap -> [(Possiblities,Possiblities)]--Map (Int,Int) [((Int,Int),Float)]
--buildWeightedMap (x:rest) = combs x (head rest)

positionWidth' :: Position -> (Int,Int) -> (Int,Int)
positionWidth' [] r = r
positionWidth' ((Finger s f):r) (m1,m2) = positionWidth' r (min m1 f, max m2 f)

positionWidth :: Position -> Int
positionWidth ((Finger s f):r) = m2 - m1
	where (m1, m2) = positionWidth' r (f, f)

positionMean' :: Position -> Int -> (Int, Int)
positionMean' [] i = (0,i)
positionMean' ((Finger _ f):r) i = (s+f,i')
	where (s, i') = positionMean' r (i+1)

positionMean :: Position -> Float
positionMean p = (fromIntegral s) / (fromIntegral i)
	where (s, i) = positionMean' p 1

positionWeighting :: Position -> Float
positionWeighting p = fromIntegral (positionWidth p) -- + positionMean p

eg1 = validPossibilities [5,0,11,14] 12 (IsChord (Chord 1 1))
eg2 = validPossibilities [5,0,11,14] 12 (IsChord (Chord 1 2))

movementWeighting :: Position -> Position -> Float
movementWeighting p1 p2 = abs (positionMean p1 - positionMean p2)

combinationWeighting :: Position -> Position -> Float
combinationWeighting p1 p2 = (1000 * movementWeighting p1 p2) + positionWeighting p1

bridge'' :: Position -> Position -> Int -> Int -> ((Int,Int),Float) 
bridge'' p1 p2 g i = ((g,i), combinationWeighting p1 p2)

bridge' :: Position -> Possiblities -> Int -> Int -> ((Int,Int),[((Int,Int),Float)])
bridge' p1 p2 i2 i1 = ((i2,i1),(mapi (\x -> bridge'' p1 x (i2+1)) p2))

bridge :: Possiblities -> Possiblities -> Int -> [((Int,Int),[((Int,Int),Float)])]-- Map (Int,Int) [((Int,Int),Float)]
bridge p1 p2 i = mapi (\x -> bridge' x p2 i) p1

weightsFromEnd :: Position -> Int -> Int -> ((Int,Int),[((Int,Int),Float)])
weightsFromEnd p g i = ((g,i),[((g+1,1), positionWeighting p)])

weightsFromStart' :: Int -> [((Int,Int),Float)]
weightsFromStart' 0 = []
weightsFromStart' i = ((2,i),0) : weightsFromStart' (i-1)

weightsFromStart :: Int -> ((Int,Int),[((Int,Int),Float)])
weightsFromStart i = ((1,1), weightsFromStart' i)

buildWeightedMap' :: PossMap -> Int -> [((Int,Int),[((Int,Int),Float)])]
buildWeightedMap' (x:[]) i = mapi (\y -> weightsFromEnd y i) x ++ [((i+1,1),[])]
buildWeightedMap' (x:r) i = bridge x (head r) i ++ buildWeightedMap' r (i+1)

buildWeightedMap :: PossMap -> M.Map (Int,Int) [((Int,Int),Float)]
buildWeightedMap m = M.fromList ((firstNode, reverse firstEdges) : buildWeightedMap' m 2)
	where (firstNode, firstEdges) = weightsFromStart $ length $ head m

optPath :: M.Map (Int,Int) [((Int,Int),Float)] -> [(Int,Int)]
optPath m = shortestPath (1,1) (fst $ fst $ M.findMax m,1) m

restOutTab = (Nothing :: Maybe String, Nothing :: Maybe Int,
	Nothing :: Maybe Int, Nothing :: Maybe Int, Nothing :: Maybe Int)

isString :: Int -> Finger -> Bool
isString i (Finger s f) = s == i

stringFret :: [Finger] -> Int -> Maybe Int
stringFret p s = if isJust r
	then Just f
	else Nothing
		where
			r = find (isString s) p
			(Finger _ f) = fromJust r

outMoment :: Position -> (Maybe String, Maybe Int, Maybe Int, Maybe Int, Maybe Int)
outMoment p = (Nothing, stringFret p 1, stringFret p 2, stringFret p 3, stringFret p 4)

convertTab' :: Tab -> PossMap -> [(Int,Int)] -> OutTab
convertTab' [] _ _ = []
convertTab' _ [] _ = []
convertTab' _ _ [] = []
convertTab' (Rest:r) m p = restOutTab : convertTab' r m p
convertTab' (t:tr) (m:mr) (p:pr) = outMoment (m !! (snd p - 1) )
	: convertTab' tr mr pr

convertTab :: Tab -> PossMap -> OutTab
convertTab t m = convertTab' t (m) (tail (optPath (buildWeightedMap m)))