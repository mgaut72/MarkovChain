SANDBOX_BD=/home/matt/documents/programming/haskell/markov/.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d

runhaskell -package-db="$SANDBOX_DB" MarkovChainFromStdin.hs 13
