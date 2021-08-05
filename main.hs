module Main (main) where

import System.Console.GetOpt
import Control.Monad
import System.IO
import System.Exit
import System.Environment

data Options = Options  { optCommand    :: String
                        , optVerbose    :: Bool
                        , optInput      :: IO String
                        , optOutput     :: String -> IO ()
                        , optPad        :: IO String
                        , optLength     :: Integer
                        }

startOptions :: Options
startOptions = Options  { optCommand    = "decrypt"
                        , optVerbose    = False
                        , optInput      = getContents
                        , optOutput     = putStr
                        , optPad        = getContents
                        , optLength     = 0
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "e" ["encrypt"]
        (NoArg
            (\opt -> return opt { optCommand = "encrypt" }))
        "Encrypt file"
    , Option "d" ["decrypt"]
        (NoArg
            (\opt -> return opt { optCommand = "decrypt" }))
        "Decrypt file (default)"
    , Option "g" ["generate"]
        (NoArg
            (\opt -> return opt { optCommand = "generate" }))
        "Generate a one-time pad"
    , Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = readFile arg })
            "FILE")
        "Input file"
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output file"
    , Option "p" ["pad"]
        (ReqArg
            (\arg opt -> return opt { optPad = readFile arg })
            "FILE")
        "One-time pad to use"
    , Option "l" ["length"]
        (ReqArg
            (\arg opt -> return opt { optLength = read arg :: Integer })
            "INT")
        "New one-time pad's length"
    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"
    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

main :: IO ()
main = do
  args <- getArgs
  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt RequireOrder options args

  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return startOptions) actions

  let Options { optCommand = command
              , optVerbose = verbose
              , optInput   = input
              , optOutput  = output
              , optPad     = pad
              , optLength  = length} = opts

  case command of
    "generate" -> output $ generate length
    "encrypt"  -> do
      inputContents <- input
      padContents <- pad
      output $ encrypt inputContents padContents
    "decrypt"  -> do
      inputContents <- input
      padContents <- pad
      output $ decrypt inputContents padContents
    _          -> putStrLn "That's not right"

generate :: Integer -> String
generate = error "implement generation, please!"

encrypt :: String -> String -> String
encrypt = error "implement encryption, please!"

decrypt :: String -> String -> String
decrypt = error "implement decryption, please!"
