module Main (main) where

import System.Console.GetOpt
import Control.Monad
import System.IO
import System.Exit
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Char

data Options = Options  { optCommand    :: String
                        , optVerbose    :: Bool
                        , optInput      :: IO T.Text
                        , optOutput     :: T.Text -> IO ()
                        , optPad        :: (IO T.Text, T.Text -> IO ())
                        , optLength     :: Int
                        }

startOptions :: Options
startOptions = Options  { optCommand    = "decrypt"
                        , optVerbose    = False
                        , optInput      = TI.getContents
                        , optOutput     = TI.putStr
                        , optPad        = (TI.getContents, TI.putStr)
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
            (\arg opt -> return opt { optInput = TI.readFile arg })
            "FILE")
        "Input file"
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = TI.writeFile arg })
            "FILE")
        "Output file (for generation, decryption, and encryption)"
    , Option "p" ["pad"]
        (ReqArg
            (\arg opt -> return opt { optPad = (TI.readFile arg,
                                                TI.writeFile arg) })
            "FILE")
        "One-time pad to use (for decryption and encryption)"
    , Option "l" ["length"]
        (ReqArg
            (\arg opt -> return opt { optLength = read arg :: Int })
            "INT")
        "New one-time pad's length (for generation)"
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
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  opts <- Prelude.foldl (>>=) (return startOptions) actions
  let Options { optCommand = command
              , optVerbose = verbose
              , optInput   = input
              , optOutput  = output
              , optPad     = (inPad, outPad)
              , optLength  = length } = opts

  case command of
    "generate" -> generate output length
    "encrypt"  -> do
      inputContents <- input
      padContents <- inPad
      output $ encrypt inputContents padContents
    "decrypt"  -> do
      inputContents <- input
      padContents <- inPad
      output $ decrypt inputContents padContents

generate :: (T.Text -> IO ()) -> Int -> IO ()
generate output length = do
  withBinaryFile "/dev/random" ReadMode
    (\handle -> do
        contents <- replicateM length $ hGetChar handle
        output $ T.pack contents)

crypt :: (Int -> Int -> Int) -> T.Text -> T.Text -> T.Text
crypt f = T.zipWith (\s1 s2 -> chr (f (ord s1) (ord s2)))

encrypt :: T.Text -> T.Text -> T.Text
encrypt = crypt (+)

decrypt :: T.Text -> T.Text -> T.Text
decrypt = crypt (-)
