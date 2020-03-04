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


--
-- Interpreters
--

runListInputForever :: [b] -> Sem (Input b ': r) a -> Sem r a
runListInputForever inputState program =
  fmap snd result -- result is of type Sem r (s, a) and we are not interested in the state, but only the value
  where
    result = runState (cycle inputState) (reinterpretInputToState program)

    reinterpretInputToState :: Sem (Input b ': r) a -> Sem (State [b] ': r) a
    reinterpretInputToState = reinterpret \case
      Input -> getOneValueFromState

    getOneValueFromState :: Member (State [b]) r => Sem r b
    getOneValueFromState = do
      gotten <- get
      let (s : ss) = gotten -- our state is infinite, so this is safe
      put ss
      pure s


prefixTrace :: Member Trace r => Sem r a -> Sem r a
prefixTrace = intercept \case
  Trace msg -> trace $ "> " ++ msg


--
-- Tie program and interpreters together
--

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
            . prefixTrace
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
           . prefixTrace
           . runInputSem (embed $ randomRIO bounds)
           . runInputSem (embed getLine)
           $ guessProg conf
  putStrLn "====== Result from IO program"
  putStrLn $ "Completion with result: " <> show iresult
