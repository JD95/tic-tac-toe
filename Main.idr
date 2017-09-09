module Main 

import Data.String 
import Data.So
import Prelude

data Space = X | O | E 

Show Space where
  show X = "X"
  show O = "O"
  show E = " "
  
notEmpty : Space -> Bool
notEmpty E = False
notEmpty _ = True

inRange : Nat -> Nat -> Nat -> Bool
inRange k j i = k <= i && i <= j

data ValidMove : (min:Nat) -> (max:Nat) -> Type where
  Move : (n:Nat) -> (s:Space) 
      -> {auto r : So (inRange min max n)} -- Requires proof that move is in range 
      -> {auto e : So (notEmpty s)}        -- Requires proof that symbol is not empty 
      -> ValidMove min max 

Show (ValidMove min max) where
  show (Move n s) = show (n, s)

getMove : (p:Space) -> (min:Nat) -> (max:Nat) -> (message:String) 
        -> {auto s : So (notEmpty p)} 
       -> IO (ValidMove min max)
getMove p min max message {s} = do
  print message
  input <- getLine
  case parsePositive {a=Nat} input of
    Nothing => getMove p min max "You did not enter a number!"
    Just n => case choose (inRange min max n) of
      -- choose takes the bool at runtime and produces
      -- the proof that n is in range
      Right _ => getMove p min max "Move not in range!"
      Left m => pure $ Move n p {r=m} {e=s} 

main : IO ()
main = do
  print "Hello World!"
  move <- getMove X 0 8 "Please enter move in range 0-8"
  print ("Your move " ++ show move ++ " was valid!")
