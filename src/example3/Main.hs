{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TemplateHaskell  #-}
module Main where


import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Polysemy.Trace
import System.Random (newStdGen, randomRs, randomRIO)
import Text.Read (readMaybe)

data Config = Config { maxTries :: Int
                     , lowerBound :: Int
                     , upperBound :: Int
                     } deriving Show


--
-- Interpreters
--

runListInputForever :: [i] -> Sem (Input i ': r) a -> Sem r a
runListInputForever is = fmap snd . runState (cycle is) . reinterpret \case
  Input -> do
    gotten <- get
    let (s : ss) = gotten  -- safe because the state is infinite
    put ss
    pure s


tracePrefix :: Member Trace r => Sem r a -> Sem r a
tracePrefix = intercept \case
  Trace msg -> trace $ "> " ++ msg


--
-- Program
--

doGuess :: Members '[Input String, Trace] r => Int -> Int -> Int -> Sem r Bool
doGuess toGuess maxTries remainingTries = do
  let guessnum = (maxTries - remainingTries) + 1
  trace $ "Enter guess #" <> show guessnum <> ":"
  maybeguess <- readMaybe <$> input
  case maybeguess of
    Just guess
      | guess > toGuess -> do
                             trace (show guess <> " is too high.")
                             pure False
      | guess < toGuess -> do
                             trace (show guess <> " is too low.")
                             pure False
      | otherwise -> pure True
    Nothing -> do
                 trace "That's not a valid number. You just wasted a guess!"
                 pure False


doGuesses :: Members '[Input String, Trace] r => Int -> Int -> Int -> Sem r Bool
doGuesses _ _ (-1) = do
    trace "You win!"
    pure True
doGuesses toGuess _ 0 = do
    trace $ "You ran out of guesses. Game over! The number was " <> show toGuess <> "."
    pure False
doGuesses toGuess maxTries remainingTries = do
    guessed <- doGuess toGuess maxTries remainingTries
    doGuesses toGuess maxTries (if guessed then (-1) else remainingTries - 1)


playAgain :: Members '[Input String, Trace] r => Sem r Bool
playAgain = do
    trace ""
    trace "Play again? (Y/n)"
    choice <- input
    pure (elem choice ["y", "yes", "hokay", ""])
          
          
guessProg :: Members '[Input String, Input Int, Trace] r => Config -> Sem r [Bool]
guessProg config = go True []
  where
    go :: Members '[Input String, Input Int, Trace] r => Bool -> [Bool] -> Sem r [Bool]
    go False record = pure $ reverse record
    go True record = do
      trace ""
      trace $ "I'm thinking of a number between " <> show (lowerBound config) <> " and " <> show (upperBound config) <> ". Guesses allowed: " <> show (maxTries config)
      toGuess <- input
      won <- doGuesses toGuess (maxTries config) (maxTries config)
      again <- playAgain
      trace $ if again then "Starting a new game!" else "Goodbye!"
      go again (won:record)



main :: IO ()
main = do
  rgen <- newStdGen
  let
    guessesallowed = 5
    guessmin = 1
    guessmax = 100
    bounds = (guessmin, guessmax)
    conf = Config { maxTries = guessesallowed, lowerBound = guessmin, upperBound = guessmax}
    randlist = randomRs (guessmin, guessmax) rgen
    inputlist = ["50", "75", "", "", "", "y","75"]
    
    (progoutput :: [String], progreturn)
            = run
            . runOutputList
            . traceToOutput
            . tracePrefix
            . runListInputForever randlist
            . runListInputForever inputlist
            $ guessProg conf

  putStrLn "====== Running pure program"
  mapM_ print progoutput
  putStrLn "====== Result from pure program"
  putStrLn $ "Completion with result: " <> show progreturn
  putStrLn "\n"


  putStrLn "====== Running IO program"
  iresult <- runM
           . traceToIO
           . tracePrefix
           . runInputSem (embed $ randomRIO bounds)
           . runInputSem (embed getLine)
           $ guessProg conf
  putStrLn "====== Result from IO program"
  putStrLn $ "Completion with result: " <> show iresult
