# deprecated

Use https://github.com/carlohamalainen/glacier-push-go instead.

# glacier-push

Use the amazonka API to push files to Amazon Glacier.

Building:

    cabal build
    cabal install --installdir=$HOME/opt/glacier-push-bin

Dev tools:

    ghcid -c 'cabal new-repl'

Usage:

    glacier-push-exe <vault> <file>

OSX:

On Big Sur you will need to install openssl:

    brew install openssl
    
    cabal build --extra-include-dirs=/usr/local/opt/openssl/include \
                --extra-lib-dirs=/usr/local/opt/openssl/lib

    cabal install
