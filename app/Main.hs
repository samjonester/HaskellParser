module Main where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

data Validator a b = Validator {
  check :: b -> Bool,
  err :: a
}

validate :: b -> [Validator a b] -> Either a b
validate = foldM (\val Validator{check = check, err = err} ->
                   if check val then Right val else Left err)

-- End validations -- Start application

type Error = String
type Success = String
type Validated = Either Error Success
 
main :: IO ()
main = do
  putStrLn "Hello"
  putStrLn =<< validationMessage <$> getInput

validationMessage :: Validated -> String
validationMessage (Right _) =  "Valid value thanks for playing"
validationMessage (Left err) =  "Invalid! " ++ show err

getInput :: IO Validated
getInput = (`validate` validations) <$> getLine

validations :: [Validator Error Success]
validations = [
    Validator {check = (/= "foobar"), err = "No foobar allowed."}
  , Validator {check = (< 10) . length, err = "Too long."}]

