# Git push all repos
push:
    git push -u gh-origin main
    git push -u origin main

# Stack build file-watch
build:
    stack build --fast --file-watch
    
# Stack build file-watch
b:
    stack build 

# Stack test file-watch
test: 
    stack test --file-watch

# Make GitHub CI from cabal file
make_ci:
    haskell-ci github haskMus.cabal

# Make cabal package version bounds
cabal_bounds:
    cabal gen-bounds

# Clean the project
clean:
    stack clean