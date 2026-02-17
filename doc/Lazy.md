# Bidirectional Computation using Lazy Evaluation

## Introduction

I recently came across the "bowling score programming problem" that I thought would be a great
demonstration of using lazy evaluation to perform bidirectional computation -- receiving information
from both the past and the future, and sending results back in both directions as well.

### The Problem

Given a list of 10 frames from a game of 10-pin bowling, compute the running score tally of the frames.
In bowling, each frame is an attempt to knock down as many of the 10 pins as possible, using 1 or 2
rolls of the ball.  If all 10 pins are knocked down on the first roll, it is a `Strike`. If all 10 pins
are knocked down with 2 rolls, it is called a `Spare`.  Otherwise, the two rolls result in an `OpenFrame`.

### Scoring

* `Strike`: score is 10 plus the number of pins knocked down on the next two rolls.
* `Spare`: score is 10 plus the number of pins knocked down on the next roll.
* `OpenFrame`: score is the total number of pins knocked down with the two rolls.

In the 10th frame, if the bowler bowls a `Strike` or `Spare`, they get to roll the number
of extra balls required to compute the score (2 more for a `Strike` and 1 more for a `Spare`)

### Bidirectional Information

As you can see, computing the running score tally on a frame involves information from the prior frame
(the computed running score tally up to this frame) and from the subsequent frame(s) (the pin counts for the
next two rolls).  Also, we need to send results to the future frames (the new running tally) and the
prior frame (the two rolls that it might need to compute its score). When solving this in a standard, strict
imperative language, this would normally involve some form of explicit look-ahead to get subsequent roll values,
but with lazy evaluation by default, like in Haskell, Miranda, and Admiran, the problem can be written with
implicit bidirectional information transfer.

In Haskell, this form of bidirectional "time-travelling" computation has been abstracted into the
[Tardis Monad](https://hackage.haskell.org/package/tardis-0.4.1.0/docs/Control-Monad-Tardis.html).
However, here we'll write a solution from scratch in Admiran.

### Writing a solution in Admiran

First, we define an Algebraic Data Type to hold information about each frame:

    frame ::=
        OpenFrame int int |     || OpenFrame with pin-count for the two rolls
        Spare     int     |     || Spare with pin-count for the first roll (total is implicitly 10)
        Strike            |     || Strike (total is implicitly 10)
        Extra     int           || Extra roll in the 10th frame for a Strike or Spare

To compute the running score tally for the game, we want a helper-function that performs the
bidirectional computation for each frame. It needs to get the current running tally `t` from the
prior frame and the two subsequent roll values `a` and `b` from the subsequent frames to compute the
new running tally `t'`.  It then must send `t'` to the subsequent frame's computation while sending
the two roll values back that the prior frame needs to correctly compute it's running tally. In addition,
we would like to collect the running tally in a list. This could come from either direction, but since
Admiran lists naturally append at the front, we'll get the computed running tally list from the future
subsequent frame computations, and add our computed `t'` value to the front of it, then send that to the
prior frames:

    computeFrame :: int -> [frame] -> ([int], int, int)

Let's handle the base-case first, where we are at the end of frame list.  Here we simply return an
empty running tally and two zeros to represent "rolls" with zero pins knocked down:

    computeFrame t [] = ([], 0, 0)

Now lets perform the computation for each frame type.  In Admiran, we use pattern-matching to destructure
a constructed value into its components.  For the `OpenFrame` case, we add the two balls `a` and `b` from
the `OpenFrame` to the current tally `t` from the prior frame and recursively call computeFrame on the rest
of the frame list, getting back a list of running frame tallies to which we append our computed tally `t'`
and return it, plus the two ball counts `a` and `b`:

    computeFrame t (OpenFrame a b : fs)
        = (t' : ts, a, b)
          where
            t'         = t + a + b
            (ts, _, _) = computeFrame t' fs

A `Spare` is computed similarly, but needs the next roll value from the subsequent frame:

    computeFrame t (Spare a : fs)
        = (t' : ts, a, 10 - a)
          where
            t'         = t + 10 + b
            (ts, b, _) = computeFrame t' fs

and a `Strike` needs the next two roll values from the subsequent frame(s):

    computeFrame t (Strike : fs)
        = (t' : ts, 10, a)
          where
            t'         = t + 10 + a + b
            (ts, a, b) = computeFrame t' fs

Finally, to handle `Extra` rolls during the 10th frame, we tally them similarly to a `Spare`, but don't append
the computed tally to the tally list (as they are implicitly an extension of the 10th frame):

    computeFrame t (Extra a : fs)
        = (ts, a, b)
          where
            t'         = t + a
            (ts, b, _) = computeFrame t' fs

Now all we need to do is "prime the pump" with an initial 0 tally and extract the final running tally list from the
result:

    tally :: [frame] -> [int]
    tally fs = computeFrame 0 fs |> \(ts, _, _) -> ts

Let's try it out on an example game:

    main :: io ()
    main = tally game |> showlist showint |> putStrLn
           where
             game = [ OpenFrame 1 4, OpenFrame 4 5, Spare 6, Spare 5, Strike, OpenFrame 0 1
                    , Spare 7, Spare 6, Strike, Spare 2, Extra 6]

This prints the result: `[5, 14, 29, 49, 60, 61, 77, 97, 117, 133]`

We can try it out on a "perfect game" of 10 `Strike`s (and two subsequent `Strikes` in the 10th frame):

    game = rep 10 Strike ++ rep 2 (Extra 10)

Which results in `[30, 60, 90, 120, 150, 180, 210, 240, 270, 300]`.

### Wait, how does this actually work?

In Lazy (call-by-need) languages, computations are bundled up into `thunks`: functions with no arguments
that perform the computation when demanded, and then memoize it so that it simply returns the computed
value is evaluated again.  Computation is performed only when it is required, either through destructuring
a value, performing a conditional test, or printing a result.  Since all computations are lazy by default,
we can build up the bidirectional structure of the computation without initially computing any actual values
until we finally start printing the result.  This is also referred to as "tying the knot".

If we follow through the first few steps of running the initial game, the order of computations is:

* `putStrLn`, which then demands the string to print from
* `showlist showint`, which then demands the list of ints from
* `tally game`, which calls `tally` with `game`, when then calls
* `computeFrame 0 (game)`, which then destructures `game` to
* `computeFrame 0 (OpenFrame 1 4 : fs)`, which returns `(t' : ts, 1, 4) to `tally`
* back in `tally` we extract the first component `(t' : ts)` and return it
* back in `showlist showint`, we extract the first element of the list `t'` and compute it
* back in `computeFrame`, the `t'` thunk is now computed (0 + 1 + 4 = 5) and returned
* back in `showint`, we convert 5 to "5" and print it, then continue with the rest of the list `ts`.
* back in `computeFrame`, the `ts` thunk is demanded, requiring evaluation of the recursive call to `computeFrame`
