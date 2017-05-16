# Haskell Final Project
## Andrew Ksendzoff

#### Yung Lean lyrics generator based on a Markov chain.

For this project, I decided to generate rap lyrics using a Markov chain.
I developed this on Mac OS X.
I use two different approaches: a word-level Markov chain, and a character-level Markov chain.
The former generates more gramatically-correct verses, albeit less original ones.
The latter generates slightly more nonsensical verses, but they are less true to the data.


Dependencies: regex, markov-chain

Install dependencies before running.
To install dependencies, use either cabal or stack, depending on your environment.

    cabal install regex markov-chain

    stack install regex markov-chain


After dependencies have been installed, simply compile

    ghc ./LeanGen.lhs

and then execute

    ./LeanGen
