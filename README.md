# glacier-push

Use the amazonka API to push files to Amazon Glacier.

Verified to build with this version of stack:

    Version 1.6.5, Git revision 24ab0d6ff07f28276e082c3ce74dfdeb1a2ca9e9 (5514 commits) x86_64 hpack-0.20.0

Building:

    stack build
    stack install

Usage:

    glacier-push-exe <vault> <file>

OSX:

You need openssl v1.1 or later (the default installed by brew is 1.0 as of 2019-09-21):

    brew install openssl@1.1
    stack build --extra-include-dirs=/usr/local/opt/openssl@1.1/include --extra-lib-dirs=/usr/local/opt/openssl@1.1/lib
    stack install
