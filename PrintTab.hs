module PrintTab (
		printTab
	) where

import Data.List

import ConvertTab

addSpacing :: String -> String
addSpacing s = extra ++ s
	where extra = if length s == 1
		then "-"
		else ""

printNote :: Maybe Int -> String
printNote Nothing = "--"
printNote (Just n) = addSpacing(show n) 

printString :: [Maybe Int] -> String
printString [] = "\n"
printString (x:r) = (printNote x) ++ (printString r)

printTab :: OutTab -> String
printTab t = printString s1 
	++ printString s2
	++ printString s3
	++ printString s4
		where (c,s1,s2,s3,s4) = unzip5 t