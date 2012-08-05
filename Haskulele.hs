import System.Console.GetOpt
import System.IO hiding (withFile)
import System.Exit
import System.Environment
import Data.List
import Data.Char
import Control.Monad
import Text.Printf
import Data.Maybe
import Data.Map ((!), findMax, fromList, fromListWith, adjust, keys, Map)

import DecodeTab
import GeneratePossiblities
import ConvertTab
import Dijkstra
import PrintTab
import Misc

data Options = Options
 {
    input :: Maybe String,
    output :: Maybe String,
    maxFret :: Int,
    tuning :: [Int],
    transposeAmount :: String,
    autoTranspose :: Bool,
    showVersion :: Bool,
    showHelp :: Bool,
    showTuning :: Bool
 } deriving Show

defaultOptions = Options
 {  input = Nothing,
    output = Nothing,
    tuning = [9,4,0,7],
    maxFret = 12,
    transposeAmount = "0",
    autoTranspose = False,
    showVersion = False,
    showHelp = False,
    showTuning = False
  }

ukueleTunings :: Int -> [Int]
ukueleTunings i = [9,4,0,7]

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v']     ["version"]
     (NoArg (\ opts -> opts { showVersion = True }))
     "Version of Haskulele"
 , Option ['h']     ["help"]
     (NoArg (\ opts -> opts { showHelp = True }))
     "Print this help message"
 , Option ['t']     ["tuning"]
     (ReqArg (\ d opts -> opts { tuning = [9,4,0,7] }) "TUNING")
     "Output tuning, see bellow"
 , Option ['j']     ["tuning-help"]
     (NoArg (\ opts -> opts { showTuning = True }))
     "Help on tuning"
 , Option ['f']     ["fret"]
     (ReqArg (\ d opts -> opts { maxFret = read d :: Int }) "MAXFRET")
     "The maximum fret on the output"
 , Option ['T']     ["transpose"]
     (ReqArg (\ d opts -> opts { transposeAmount = d}) "SEMI-TONES")
     "Tranpose the input tab"
 , Option ['I']     ["input"]
     (ReqArg (\ d opts -> opts { input = Just d }) "FILE")
     "Input File, stdin otherwise"
 , Option ['O']     ["output"]
     (ReqArg (\ d opts -> opts { output = Just d }) "FILE")
     "Output file, stdout otherwise"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
            where header = "Usage: haskulele [-hv] [-f FRETS] [-t TUNING] [-I FILE] [-O FILE]"

tuningMessage :: String
tuningMessage = "\nThe current tuning Haskulele allows are: \n\
	\ 1 - GCEA (default)\n\
    \ 2 - ADF#B\n\
    \ 3 - DGBE\n\n\
    \Please use the number to refer to them."

main :: IO()
main = do
    (args, files) <- getArgs >>= compilerOpts
    if showHelp args
    then putStr (usageInfo "" options)
    else if showVersion args 
    then putStrLn "Haskulele v0.1b"
    else if showTuning args
    then putStrLn tuningMessage
    else do
    	input <- if isNothing (input args) 
    		then getContents
    		else readFile (fromJust (input args))
    	let tab = toValidRange $ decodeTab input
    	let pos = tabPossibilities tab (tuning args) (maxFret args)
    	let wmap = buildWeightedMap pos
        let out = convertTab tab pos
        putStrLn $ printTab out