# Guessing Game

Based on the final version of a polysemy example from https://github.com/KerfuffleV2/haskell-polysemy-test.git

## What is this program doing ?

GuessingGame is an interacting program that plays a little game. It creates a random number in a specified range and gives the user a specified number of tries to guess the number. When the user enters a number, she is told if she guessed correctly or if the number is higher or lower than the number to guess. After a game is finished - either by guessing correctly or by using all available tries - the user gets the option to start another game.

## How is this program implemented ?

The core of the program is the ```polysemy``` library, which is an implementation of extensible effects using the freer monad.

The ```polysemy``` library has several advantages over other effect libraries, such as ```extensible-effects```, ```fused-effects```, ```freer-effects``` and ```freer-simple```:

- it is fast
- it requires no boilerplate code to create new effects
- writing interpreters is very easy (no messy type signatures)
- it is actively maintained
- it comes with a lot of effects out of the box
- it has meaningful error messages
- it is good at type inference

## Why deal with effects at all?

Effects are there to solve the problems we have with monads. The first and most prominent problem is: monads are not composable. 
To compose monads, we have to use a hack called monad transformers. A second problem is, that functions working with IO are not testable.
So we have to work really hard to push IO to the boundary of our program.

Sandy Maguire gave a very good example in his talk about [freer monads presented at BayHack 2017](https://www.youtube.com/watch?v=gUPuWHAt6SA).

In this talk, Sandy uses the library ```freer-effects``` to demonstrate how free monads and extensible effects 
can help us to write testable programs. The video is 45 minutes long. Please watch it.

Because Sandy was not happy with several aspects of the ```freer-effects``` library, he wrote his own: [polysemy](https://hackage.haskell.org/package/polysemy).

[This talk](https://youtu.be/idU7GdlfP9Q?t=1394) gives a good introduction to ```polysemy```, and
[this talk](https://www.youtube.com/watch?v=-dHFOjcK6pA) gives a more advanced explanation about what makes ```polysemy``` special.
