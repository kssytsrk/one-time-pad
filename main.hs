import System.Environment
import Data.List

main :: IO ()
main = do
  (command:options) <- getArgs
  case command of
    "generate" -> putStrLn $ "implement generating, please! command " ++ command
                  ++ ", options " ++ concat options
    "encrypt"  -> putStrLn "implement encryption, please!"
    "decrypt"  -> putStrLn "implement decryption, please!"
    _          -> putStrLn "That's not right"
