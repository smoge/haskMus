

# git push github bitbucket
push:
    git push -u gh-origin main
    git push -u origin main

# stack build file-watch
build:
    stack build --fast --file-watch

# stack test file-watch
test: 
    stack test --file-watch

# make github ci from cabal file
make-ci:
    haskell-ci github hasMusic.cabal

cabal-bounds:
    cabal gen-bounds    