ag -l -G '\.hs$|\.cabal$|\.elm$|.yaml$' | entr sh -c 'make build'
