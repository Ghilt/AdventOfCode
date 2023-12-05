Command line run kotlin:

kotlinc AoC_2019_d2.kt -include-runtime -d aoc.jar | java -jar aoc.jar

kotlin commandline setup: 

You need to configure your environment variables, JAVA_HOME and PATH. (take java jdk download from intellij and point to that)
Then you need to download the kotlin compiler and add that to PATH as well
https://kotlinlang.org/docs/command-line.html

Haskel not installed att all?:

Simple one line install script to install all required components can be found:

https://www.haskell.org/ghcup/#

The script is:
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
Just run it in powershell.


Commandline run haskell:

runhaskell .\AoC_2020_d9.hs

If imports are failing for haskell they need to be installed with cabal:

cabal install --lib Unique
cabal install --lib split

I do not know how to find the name of these packages that cabal will understand. https://hackage.haskell.org/packages/browse

commit message template: 
Add solution for day x, part y, 2022