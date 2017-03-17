echo "make sure you have ghc and cabal installed"
cabal install coderworld-api
cabal install blank-canvas
ghc --make StepUtil.hs
ghc --make Shapes.hs
ghc --make DataTypes.hs
