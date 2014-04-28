In order to build this project using a cabal sandbox, you first need to do

cabal sandbox init
cabal install --only-dependencies --enable-tests

Otherwise, even 'cabal configure' will fail complaining that it cannot
find dependencies. The --enable-tests flag is needed so that
dependencies specific to the tests get installed too. Without it a
'cabal test' will fail complaining that it cannot find the necessary
dependencies.
