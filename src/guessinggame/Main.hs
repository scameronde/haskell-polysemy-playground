{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TemplateHaskell  #-}
module Main where


import Polysemy
import Polysemy.Input
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
          
guessProg :: Members '[Input String, Input Int, Trace] r => Config -> Sem r [Bool]
guessProg config = go True []
  where
    go :: Members '[Input Int, Input String, Trace] r => Bool -> [Bool] -> Sem r [Bool]
    go False gameResults = pure (reverse gameResults)
    go True record = do
      trace ""
      trace $ "I'm thinking of a number between " <> show (lowerBound config) <> " and " <> show (upperBound config) <> ". Guesses allowed: " <> show (maxTries config)
      numberToGuess <- input
      won <- doGuesses numberToGuess (maxTries config) (maxTries config)
      again <- wantToPlayAgain
      trace (if again then "Starting a new game!" else "Goodbye!")
      go again (won:record)


    doGuesses :: Members '[Input String, Trace] r => Int -> Int -> Int -> Sem r Bool
    doGuesses _ _ (-1) = do
        trace "You win!"
        pure True
    doGuesses toGuess _ 0 = do
        trace $ "You ran out of guesses. Game over! The number was " <> show toGuess <> "."
        pure False
    doGuesses numberToGuess allowedTries remainingTries = do
        guessed <- doGuess numberToGuess (allowedTries - remainingTries +1)
        doGuesses numberToGuess allowedTries (if guessed then (-1) else remainingTries - 1)


    doGuess :: Members '[Input String, Trace] r => Int -> Int -> Sem r Bool
    doGuess numberToGuess guessCount = do
      trace $ "Enter guess #" <> show guessCount <> ":"
      maybeguess <- readMaybe <$> input
      case maybeguess of
        Just guess
          | guess > numberToGuess -> do
                                 trace (show guess <> " is too high.")
                                 pure False
          | guess < numberToGuess -> do
                                 trace (show guess <> " is too low.")
                                 pure False
          | otherwise -> pure True
        Nothing -> do
                     trace "That's not a valid number. You just wasted a guess!"
                     pure False


    wantToPlayAgain :: Members '[Input String, Trace] r => Sem r Bool
    wantToPlayAgain = do
        trace ""
        trace "Play again? (Y/n)"
        choice <- input
        pure (elem choice ["y", "yes", "okay", ""])


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
  let
    guessesallowed = 5
    guessmin = 1
    guessmax = 100
    conf = Config { maxTries = guessesallowed, lowerBound = guessmin, upperBound = guessmax}

  putStrLn "====== Running pure program"
  rgen <- newStdGen
  let
    listOfNumbersToGuess = randomRs (guessmin, guessmax) rgen
    listOfCannedAnswers = ["50", "75", "", "", "", "y","75"]
    
    (progoutput :: [String], pureResult)
            = run
            . runTraceList
            . prefixTrace
            . runListInputForever listOfNumbersToGuess
            . runListInputForever listOfCannedAnswers
            $ guessProg conf
  mapM_ print progoutput
  putStrLn "====== Result from pure program"
  putStrLn $ "Completion with result: " <> show pureResult
  putStrLn "\n"


  putStrLn "====== Running IO program"
  ioResult <- runM
           . traceToIO
           . prefixTrace
           . runInputSem (embed $ randomRIO (guessmin, guessmax))
           . runInputSem (embed getLine)
           $ guessProg conf
  putStrLn "====== Result from IO program"
  putStrLn $ "Completion with result: " <> show ioResult

