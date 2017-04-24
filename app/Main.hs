module Main where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

type Error = String
type Success = String

data Validator a = Validator {
  check :: a -> Bool,
  err :: Error
}

main :: IO ()
main = do
  putStrLn "Hello"
  output <- runExceptT getInputE
  case output of
    Right _  -> putStrLn "Valid value thanks for playing"
    Left err -> putStrLn $ "Invaid! " ++ show err

getInputE :: ExceptT Error IO Success
getInputE = do
  input <- lift getLine
  either throwE return $ validateInput input

validateInput :: String -> Either Error Success
validateInput = validate [
    Validator {check = (/= "foobar"), err = "No foobar allowed."}
  , Validator {check = (< 10) . length, err = "Too long."}]

validate :: [Validator r] -> r -> Either Error r
validate checks input = foldl validate' (Right input) checks
  where
    validate' acc Validator{check = check, err = err} =
        acc >>= (\val -> if check val then Right val else Left err)
