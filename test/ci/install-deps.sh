#!/bin/bash

set -x
set -e

# Install Tidy everywhere
case $TRAVIS_OS_NAME in
"linux")
  sudo add-apt-repository -y ppa:robert7/tidy-html5
  sudo apt-get update
  sudo apt-get install tidy
;;

"osx")
  brew install tidy-html5
  brew link tidy-html5
;;
esac

# Install esy or opam
if [[ $ESY_BUILD == YES ]]; then
  npm --global install esy@0.3.x
else
  OPAM_RELEASES=https://github.com/ocaml/opam/releases/
  OPAM_VERSION=2.0.1
  OPAM_PKG=opam-${OPAM_VERSION}-x86_64

  case $TRAVIS_OS_NAME in
  "linux")
    wget ${OPAM_RELEASES}/download/${OPAM_VERSION}/${OPAM_PKG}-linux
    sudo mv ${OPAM_PKG}-linux /usr/local/bin/opam
    sudo chmod a+x /usr/local/bin/opam
  ;;

  "osx")
    brew install opam
    brew link opam
  ;;
  esac

  opam init -y --compiler=$OCAML --disable-sandboxing
fi
