Command line run kotlin:

kotlinc AoC_2019_d2.kt -include-runtime -d aoc.jar | java -jar aoc.jar

kotlin commandline setup: 

You need to configure your environment variables, JAVA_HOME and PATH. (take java jdk download from intellij and point to that)
Then you need to download the kotlin compiler and add that to PATH as well
https://kotlinlang.org/docs/command-line.html

Commandline run haskell:

runhaskell .\AdventOfCode2020\AoC_2020_d9.hs

If imports are failing for haskell they need to be installed with cabal:

cabal install --lib Unique
cabal install --lib split

I do not know how to find the name of these packages that cabal will understand. https://hackage.haskell.org/packages/browse