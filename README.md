# glacier-push

Use the amazonka API to push files to Amazon Glacier.

Building:

    cabal v2-build
    cabal v2-install --installdir=$HOME/opt/glacier-push-bin

Dev tools:

    ghcid -c 'cabal new-repl'

Usage:

    glacier-push-exe <vault> <file>

OSX:

You need openssl v1.1 or later (the default installed by brew is 1.0 as of 2019-09-21):

    brew install openssl@1.1
    stack build --extra-include-dirs=/usr/local/opt/openssl@1.1/include --extra-lib-dirs=/usr/local/opt/openssl@1.1/lib
    stack install
