-- ---------------------------------------------------------------------------
-- | This Module is responsible for taking a Tab (as defined and generated 
-- from DecodeTab) and converting it into a tab playable on a ukulele. It does
-- this by generating all possible methods of playing the given notes or chord
-- on a ukulele, and then rating how easy they are to play and how easy it is
-- to move from one to another

module GeneratePossiblities (
	Finger (Finger),
	Position,
	Possiblities,
	PossMap,
	tabPossibilities,
	toValidRange,
	validPossibilities  )
		where --Take out validPosi

import System.Environment
import System.FilePath
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char
import qualified Data.Map as M

import DecodeTab
import Misc

data Finger = Finger Int Int deriving (Show, Eq, Ord) -- String Fret
type Position = [Finger]
type Possiblities = [Position]
type PossMap = [Possiblities] -- The nth moments possibilities
type PossInfo = (PossMap,[Int])

advanceIntervalList :: [Int] -> [Int]
advanceIntervalList (x:r) = r ++ [x + 12]

--possiblePossitionsForNotes :: [Int] -> Possibilities
chordNotesPossibleOnString :: [Int] -> Int -> Int -> [(Int,Int)] -- NotesInChord -> StringInterval -> Interation -> NumOfFrets
chordNotesPossibleOnString n m i
	| head n < m = (head n, mod i 4) : chordNotesPossibleOnString (advanceIntervalList n) m (i+1) -- (Fret, note index)
	| otherwise = []

normaliseNotes :: [Int] -> [Int]
normaliseNotes n = sort (map (\x -> mod x 12) n)

noramlisedTranslate :: [Int] -> Int -> [Int]
noramlisedTranslate n a = normaliseNotes (map (\x -> x - a) n)

chordNotes :: Chord -> [Int]
chordNotes (Chord k q) = noramlisedTranslate (getUkuleleQuality q) k

allChordPossibilities :: [Int] -> [Int] -> Int -> [[(Int,Int)]]
allChordPossibilities _ [] _ = []
allChordPossibilities n (t:tr) m = chordNotesPossibleOnString (noramlisedTranslate n t) m 1 : allChordPossibilities n tr m

maintainAvailable :: [Int] -> Int -> Int -> [Int]
maintainAvailable p d i
 | length p == i = delete d p
 | length p < i = p
 | otherwise = error "maintainAvailable has bad input."

func :: [[(Int,Int)]] -> Position -> [Int] -> Int -> Possiblities -- String pos (fret, Note num) -> past -> aval -> ItNum
func [] p _ _ = [p]
func ([]:sr) _ _ _  = []
func (((f,n):nr):sr) p a i = if elem n a
	then (func sr (p ++ [Finger i f]) (maintainAvailable a n i) (i-1)) ++ (func (nr:sr) p a i)
	else func (nr:sr) p a i

validChordPossibilities :: Chord -> [Int] -> Int -> Possiblities -- Int is the max fret
validChordPossibilities c t m = map sort (func strPos [] [0..maxPos] 4)
	where
		strPos = allChordPossibilities (chordNotes c) t m
		maxPos = length (chordNotes c) - 1

--validNotePossibilities 

notesPossibleOnString :: [Int] -> Int -> Int -> Int -> [(Int,Int)] --NotesInChord -> StringInterval -> Iteration -> NumOfFrets
notesPossibleOnString [] _ _ _ = []
notesPossibleOnString (x:z) i t m
 | (x >= t && x <= t + m) = (x-t,i) : notesPossibleOnString z (i+1) t m
 | otherwise = notesPossibleOnString z (i+1) t m

func2 :: [[(Int,Int)]] -> Position -> Notes -> Int -> Possiblities
func2 [] p [] _ = [p]
func2 [] p _ _ = []
func2 ([]:sr) p l i  = func2 sr p l (i-1)
func2 (((f,n):nr):sr) p l i = if elem n l
	then (func2 sr (p ++ [Finger (5-i) f]) (delete n l) (i-1)) ++ func2 (nr:sr) p l i
	else func2 (nr:sr) p l i


allNotePossibilities :: [Int] -> [Int] -> Int -> [[(Int,Int)]]
allNotePossibilities _ [] _ = []
allNotePossibilities n (t:tr) m = thisString : nextStrings
	where
		thisString = notesPossibleOnString n 0 t m
		nextStrings = allNotePossibilities n tr m

validNotePossibilities :: [Int] -> [Int] -> Int -> Possiblities
validNotePossibilities n t m = func2 (allNotePossibilities n t m) [] [0..(length n)-1] 4

type MomentLib = M.Map Moment Possiblities

test :: Int -> Int
test a = a + 2

validPossibilities :: [Int] -> Int -> Moment -> Possiblities
validPossibilities t m (IsChord c) = validChordPossibilities c t m
validPossibilities t m (IsNotes n) = validNotePossibilities n t m

tabPossibilities' :: Tab -> [Int] -> Int -> M.Map Moment Possiblities -> PossMap
tabPossibilities' [] _ _ _ = []
tabPossibilities' (Rest:r) t m db = tabPossibilities' r t m db
tabPossibilities' (x:r) t m db = z : tabPossibilities' r t m db'
	where (z, db') = cached (validPossibilities t m) x db

tabPossibilities :: Tab -> [Int] -> Int -> PossMap
tabPossibilities tab tun m = tabPossibilities' tab tun m M.empty


boundryComp :: (Int -> Int -> Int) -> Int -> Maybe Int -> Maybe Int
boundryComp comp a Nothing = Just a
boundryComp comp a (Just b) = Just (comp a b)

tabBoundry' :: Tab -> (Maybe Int, Maybe Int)
tabBoundry' [] = (Nothing, Nothing)
tabBoundry' (Rest:rest) = tabBoundry' rest
tabBoundry' (IsChord c:rest) = tabBoundry' rest
tabBoundry' (IsNotes n:rest) = (boundryComp min (head n) cmin, boundryComp max (last n) cmin)
	where (cmin,cmax) = tabBoundry' rest

tabBoundry :: Tab -> (Int, Int)
tabBoundry t = (fromJust cmin, fromJust cmax)
	where (cmin,cmax) = tabBoundry' t

octiveNum :: Int -> Int
octiveNum n
	| n < 12 = 0
	| otherwise = octiveNum (n-12) + 1 

toValidRange' :: [Moment] -> Int -> [Moment]
toValidRange' [] i = []
toValidRange' (Rest:r) i = toValidRange' r i
toValidRange' (IsChord c:r) i = toValidRange' r i
toValidRange' (IsNotes n:r) i = IsNotes (map (\x ->  x - i) n) : toValidRange' r i

toValidRange :: Tab -> Tab
toValidRange t = if cmax - cmin > 24
	then error "The range of notes in the song is too wide to play on a ukulele"
	else toValidRange' t (octiveNum cmin * 12)
		where (cmin,cmax) = tabBoundry t