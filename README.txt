Command line run kotlin:

kotlinc AoC_2019_d2.kt -include-runtime -d aoc.jar | java -jar aoc.jar

Commandline run haskell:

runhaskell .\AdventOfCode2020\AoC_2020_d9.hs

If imports are failing for haskell they need to be installed with cabal:

cabal install --lib Unique
cabal install --lib split

I do not know how to find the name of these packages that cabal will understand. https://hackage.haskell.org/packages/browse