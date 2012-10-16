module FilterObsCmdLnOpts where

import Control.Monad			(foldM)
import Data.Char			(toLower)
import System.Console.GetOpt		(ArgOrder (..), ArgDescr (..),
					 OptDescr (..), getOpt, usageInfo)
import System.Environment		(getProgName)


data CmdLnOpts = CmdLnOpts
	{ filtDiscovFlag :: Bool
	, outputUsage :: Bool -- ^ Should usage information be output.
	}

defaultCmdLnOpts = CmdLnOpts
	{ filtDiscovFlag = False
	, outputUsage = True
	}

cmdLnOpts :: [OptDescr (CmdLnOpts -> IO CmdLnOpts)]
cmdLnOpts =
	[ Option "d" ["by-discovery-flag"]
	    ( OptArg (\v opts -> return $ opts {
	    			filtDiscovFlag = maybe True parseBool v })
		     "on|off" )
	    "Output the discovery observation for each object (instead of the earliest observation)."
	, Option "h" ["help"]
	    ( NoArg (\opts -> do showHelp
	    			 return $ opts { outputUsage = False }) )
	    "Show a command-line help message."
	]

parseBool :: String -> Bool
parseBool v = (map toLower v) `elem` ["true", "on", "1", "yes", "+", "sure",
	"whynot", "maybe"]

showUsage :: IO ()
showUsage = do
	progName <- getProgName
	let outpHeader = "Usage:\n" ++ progName ++
			 " [-d] inputFilePath [... inputFilePath]\n"
	putStrLn $ usageInfo outpHeader cmdLnOpts

showHelp :: IO ()
showHelp = do
	progName <- getProgName
	putStrLn $ progName ++ " Outputs only one observation for each celestial object."
	putStrLn $ " Without any arguments the earliest observation is selected."
	putStrLn ""
	showUsage

parseArgV :: [String] -> IO (CmdLnOpts, [String])
parseArgV args =
	case getOpt Permute cmdLnOpts args of
	  (o, n, [])   -> do
	  	opts <- foldM (flip id) defaultCmdLnOpts o
		resOpts <- procsOpts opts
		return (resOpts, procsN n)
	  (_, _, errs) -> do showUsage
			     ioError $ userError $ concat errs
  where procsOpts opts0 = return opts0 -- We need no option processing.
  	procsN [] = ["../t1-read/NumObs.txt.gz", "../t1-read/UnnObs.txt.gz"]
	procsN l  = l
