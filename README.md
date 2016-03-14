## About random-state
This is a collection of pseudo random number generators (PRNGs). While Common Lisp does provide a `RANDOM` function, it does not allow the user to pass an explicit SEED, nor to portably exchange the random state between implementations. This can be a headache in cases like games, where a controlled seeding process can be very useful.

## How To
For both curiosity and convenience, this library offers multiple algorithms to generate random numbers, as well as a bunch of generally useful methods to produce desired ranges.

    (loop with generator = (random-state:make-generator :mersenne-twister-32 123)
          repeat 10
          collect (random-state:random-int generator 0 10))
    => (5 6 6 3 2 8 6 9 6 1)

The library does make some efforts to be reasonably efficient while preserving usability, but each of the implementations could be optimised further. For performance critical sections, you might want to take matters into your own hands and only rely on `random-byte` for the most direct access to the generator. If that still is not sufficiently fast, creating a tailored implementation of an algorithm based on the source code of this library should not be difficult.
