# Guessing Game

Based on the final version of a polysemy example from https://github.com/KerfuffleV2/haskell-polysemy-test.git

## What is this program doing ?

GuessingGame is an interacting program that plays a little game. It creates a random number in a specified range and gives the user a specified number of tries to guess the number. When the user enters a number, she is told if she guessed correctly or if the number is higher or lower than the number to guess. After a game is finished - either by guessing correctly or by using all available tries - the user gets the option to start another game.

## How is this program implemented ?

The core of the program is the ```polysemy``` library, which is an implementation of extensible effects using the freer monad.

The ```polysemy``` library has several advantages over other effect libraries, such as ```extensible-effects```, ```fused-effects``` and ```freer-simple```:

- it is fast
- it requires no boilerplate code to create new effects
- it is actively maintained
- it comes with a lot of effects out of the box
- it has meaningful error messages
- it is good at type inference

... to be continued ...



