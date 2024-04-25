# Git push all repos
# git push -u gh-origin main
push:
    git push -u origin $(git rev-parse --abbrev-ref HEAD)

# Stack build file-watch
build:
    stack build --fast --file-watch

# Stack test file-watch
test:
    stack test --file-watch

# Make GitHub CI from cabal file
make_ci:
    haskell-ci github haskMus.cabal

# Build the project
build_w:
    stack build --fast --file-watch

# Make cabal package version bounds
bounds:
    cabal gen-bounds

# Clean the project
clean:
    stack clean --full

# Format Haskell project
format:
    @echo "Formating the Haskell project (ormolu)..."
    find ./src -name '*.hs' | xargs ormolu -i

# docs
# stack haddock --haddock-arguments --theme="./my.css"
docs:
    stack haddock
