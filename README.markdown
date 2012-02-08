Before you start development, please set up your pre-commit hook. You can do
this by either copying scripts/pre-commit to .git/hooks/pre-commit, or simply
making a symlink. This will ensure that the project builds and passes all tests
before being committed.

## Dependencies

- Haskell
- OpenGL
- A shit ton of haskell packages.

## Building

To build this project, You need to do the standard configure-build-test cycle.

    cabal configure --enable-tests --prefix=/absolute/path/to/checkout/inst
    cabal build # wait...
    cabal test

If you hit failures in configure, it is most likely due to unsatisfied
dependencies. Fix this by running `cabal install` for each one.

    cabal install stm filepath # ...etc

With build warnings and errors, please report it to the bugtracker on github.
For test failures, well, report those too.

## Running

After you have successfully built and test the game, you can start playing with
it by running:

    cabal copy

This copies all distribution files into the inst/ subdirectory. Now you can run
the game with:

    inst/bin/Chess [flags]

And voila! You should be good to go.
