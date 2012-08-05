-- ---------------------------------------------------------------------------
-- Program name

-- | Definition of the code

module DecodeTab (
	decodeTab,
	getUkuleleQuality,
	Notes,
	Key, 
	Chord(Chord),
	Moment(Rest, IsChord, IsNotes),
	Tab )
		where

import System.Environment
import System.FilePath
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char

-- ---------------------------------------------------------------------------
-- | This section of code is concerned with parsing a tab and converting it
-- into defined data types. Each tab is defined as a 'Tab' which is a list of
-- 'Moment's. A Moment describes the notes or chord which are played at a
-- in time. These moments are either a rest (ie no notes played), a list of
-- notes or a chord. Notes are stored as integers and chords are stored as
-- a key and a quality (ie. C, Major), both of these are stored as numbers
-- which correspond to the indices of library entries.

-- | The datatypes which store the inputted tab
type Notes = [Int]
type Key = Int
type Quality = Int
data Chord = Chord Key Quality deriving (Show, Eq, Ord)
data Moment = IsNotes [Int] | IsChord Chord | Rest deriving (Show, Eq, Ord)
type Tab = [Moment]

-- | The datatypes which hold the library of data. Each chord has notes which
-- much appear, that could possibly appear, and the notes which will be played
-- on the ukulele
type MandatoryNotes = [Int]
type OptionalNotes = [Int]
type UkuleleNotes = [Int]
data QualityEntry = QualityEntry Int MandatoryNotes OptionalNotes UkuleleNotes String deriving (Show) 

-- | The library data
keyData = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]
qualityData = [QualityEntry 0 [0,4,7] [] [0,4,7] "",
				QualityEntry 1 [0,4,7,9] [] [0,4,7,9] "M6",
				QualityEntry 2 [0,4,7,11] [] [0,4,7,11] "M7",
				QualityEntry 3 [0,2,4,7,11] [] [0,2,4,7] "M9",
				QualityEntry 4 [0,4,5,7,11] [2] [0,5,7,11] "M11",
				QualityEntry 5 [0,3,7] [] [0,3,7] "m",
				QualityEntry 6 [0,3,7,9] [] [0,3,7,9] "m6",
				QualityEntry 7 [0,3,7,11] [] [0,3,7,11] "m7",
				QualityEntry 8 [0,2,3,7] [] [0,2,3,7] "m9"]

tuningIntervals = [24,19,15,10,5,0]
--[0, 5, 10, 15,19, 24]

ukuleleIntervals :: [Int]
ukuleleIntervals = [9,4,0,7]

type MString = [Maybe Int]

-- | A getter for the name of a Quality
showQualityEntry :: QualityEntry -> String
showQualityEntry (QualityEntry i m o u n) = n

getUkuleleQuality :: Int -> [Int]
getUkuleleQuality n = u
	where (QualityEntry _ _ _ u _) = getQualityEntry n

-- | Gets the nth Quality in the library
getQualityEntry :: Int -> QualityEntry
getQualityEntry i = qualityData !! i

-- | A human readable string for a Chord
showChord :: Chord -> String
showChord (Chord k q) = (keyData !! k) ++ (showQualityEntry (getQualityEntry q))

extractFirstElement' :: [String] -> ([Char], [String])
extractFirstElement' [] = ([], [])
extractFirstElement' (first:rest) = do
	let result = extractFirstElement' rest
	((head first) : (fst result), (tail first) : (snd result))

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

-- | Replaces all x elements with y in a list
replace :: Int -> Int -> [Int] -> [Int]
replace x y [] = []
replace x y (v:w) = (if x == v then y  else v ):(replace x y w)

-- | Evaluates whether a is a subset of b, also returns a - b
isSubset :: [Int] -> [Int] -> (Bool, [Int])
isSubset [] [] = (True, [])
isSubset a [] = (False, [])
isSubset [] b = (True, b)
isSubset (a:at) (b:bt) = if a == b
	then isSubset at bt
	else
		if a < b
		then (False, [])
		else (fst result, b:snd result)
			where result = isSubset (a:at) bt

-- | Determines whether the notes are in are in the the given
-- Quality in the current key
isQuality :: [Int] -> QualityEntry -> Bool
isQuality notes (QualityEntry i m o u n) = (fst mand) && (fst opti)
	where
		mand = isSubset m notes
		opti = isSubset (snd mand) o

-- | Transposes the notes down (while keeping between 0 and 11)
-- so that the smallest non-zero note is now zero
nextTranspose :: [Int] -> ([Int], Int)
nextTranspose notes = (sort (map (\x -> x - m) notes2), m)
	where
		notes2 = sort (replace 0 12 notes)
		m = minimum notes2
	

-- | Checks every Quality in the library to see if the the notes part of that
-- the current transformation. 
checkKey :: [Int] -> [QualityEntry]-> Maybe Int
checkKey a [] = Nothing
checkKey a ((QualityEntry i m o u n):c) = if isQuality a (QualityEntry i m o u n)
	then Just i
	else checkKey a c

-- | Converts a list of notes into a chord
decodeChord' :: [Int] -> Int -> Chord
decodeChord' notes k = if k >= 12
	then error ("Unable to find chord for notes with interval " ++ show notes ++ ".")
	else if isNothing keyCheckResult
		then decodeChord' nextNotes (trans+k)
		else Chord k (fromJust keyCheckResult)
			where
				keyCheckResult = checkKey notes qualityData
				(nextNotes, trans) = nextTranspose notes

decodeChord :: [Int] -> Chord
decodeChord notes = decodeChord' (unique (sort (map (\x -> mod x 12) notes))) 0

charToInt :: Char -> Int
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt '0' = 0

 -- | Removes the first n elements from a list
cutOut :: Int -> [a] -> [a]
cutOut n [] = []
cutOut 0 x = x
cutOut n (x:y) = cutOut (n-1) y

-- | Removes all Nothings in a list of Maybes
deleteNothings :: [Maybe a] -> [a]
deleteNothings [] = []
deleteNothings (Nothing:rest) = deleteNothings rest
deleteNothings (Just x:rest) = x:deleteNothings(rest) 

-- | Removes all duplicates in a list. It assumes the list is sorted
unique :: [Int] -> [Int]
unique [] = []
unique (x:[]) = [x]
unique (x:rest) = if x == (head rest)
	then unique rest
	else x : unique rest

-- | Converts a text string into an MString
decodeString :: String -> Maybe Int -> MString
decodeString [] p = [p]
decodeString ('\n':r) p = [p]
decodeString (x:rstr) p =
	case isDigit x of 
		True -> case isJust p of 
			True -> (Nothing:decodeString rstr (Just ((10*(fromJust p)+(charToInt x)))))
			False -> (Nothing:decodeString rstr (Just (charToInt x)))
		False -> case isJust p of 
			True -> (p:decodeString rstr Nothing)
			False -> (Nothing:decodeString rstr Nothing)

decodeStrings' :: String -> [MString]
decodeStrings' [] = []
decodeStrings' s = (string:decodeStrings' (cutOut (length string) s))
	where string = decodeString s Nothing

decodeStrings :: String -> [MString]
decodeStrings s = map tail (decodeStrings' s)

addTuning' :: MString -> [Int] -> MString
addTuning' [] n = []
addTuning' (x:y) (v:w) = if isJust x
	then (Just (fromJust x+v) : addTuning' y w)
	else (Nothing : addTuning' y w)

addTuning :: [MString] -> [Int] -> [MString]
addTuning [] t = []
addTuning (x:y) t = addTuning' x t : addTuning y t

decodeTab' :: [[Maybe Int]] -> [Moment]
decodeTab' [] = []
decodeTab' (notes:rest) = if length cleaned == 0
	then Rest : decodeTab' rest
	else
		if length cleaned > 2
			then IsChord (decodeChord cleaned) : decodeTab' rest
			else IsNotes cleaned : decodeTab' rest
				where cleaned = unique (sort (deleteNothings notes))

decodeTab :: String -> [Moment]
decodeTab str = decodeTab' transformed
	where
		strings = decodeStrings str
		transformed = addTuning (transpose strings) tuningIntervals