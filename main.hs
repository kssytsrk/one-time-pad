module Main (main) where

import System.Console.GetOpt
import Control.Monad
import System.IO
import System.Exit
import System.Environment
import Data.Text
import qualified Data.Text.IO as T

data Options = Options  { optCommand    :: String
                        , optVerbose    :: Bool
                        , optInput      :: IO Text
                        , optOutput     :: Text -> IO ()
                        , optPad        :: IO Text
                        , optLength     :: Int
                        }

startOptions :: Options
startOptions = Options  { optCommand    = "decrypt"
                        , optVerbose    = False
                        , optInput      = T.getContents
                        , optOutput     = T.putStr
                        , optPad        = T.getContents
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
            (\arg opt -> return opt { optInput = T.readFile arg })
            "FILE")
        "Input file"
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = T.writeFile arg })
            "FILE")
        "Output file"
    , Option "p" ["pad"]
        (ReqArg
            (\arg opt -> return opt { optPad = T.readFile arg })
            "FILE")
        "One-time pad to use"
    , Option "l" ["length"]
        (ReqArg
            (\arg opt -> return opt { optLength = read arg :: Int })
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
  opts <- Prelude.foldl (>>=) (return startOptions) actions

  let Options { optCommand = command
              , optVerbose = verbose
              , optInput   = input
              , optOutput  = output
              , optPad     = pad
              , optLength  = length} = opts

  case command of
    "generate" -> generate output length
    "encrypt"  -> do
      inputContents <- input
      padContents <- pad
      output $ encrypt inputContents padContents
    "decrypt"  -> do
      inputContents <- input
      padContents <- pad
      output $ decrypt inputContents padContents

generate :: (Text -> IO ()) -> Int -> IO ()
generate output length = do
  withBinaryFile "/dev/random" ReadMode
    (\handle -> do
        contents <- replicateM length $ hGetChar handle
        output $ pack contents)

encrypt :: Text -> Text -> Text
encrypt = error "implement encryption, please!"

decrypt :: Text -> Text -> Text
decrypt = error "implement decryption, please!"
