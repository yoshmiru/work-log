nix-shell -p ag -p entr --run "ag -l -G '\.hs$|\.cabal$|\.elm$|.yaml$' | entr sh -c 'make build'"
