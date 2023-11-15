# Git push all repos
# git push -u gh-origin main
push:
    git push -u origin main

# Stack build file-watch
build:
    stack build --fast --file-watch

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
    
# Format Haskell project with fourmolu
format:
    @echo "Formating the Haskell project (fourmolu)..."
    fourmolu -i ./src/*/*
    fourmolu -i ./test/*

# docs
# stack haddock --haddock-arguments --theme="./my.css"
docs:
    stack haddock 