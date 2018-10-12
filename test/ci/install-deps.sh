#!/bin/bash

# Install Tidy everywhere
case $TRAVIS_OS_NAME in
"windows") choco install html-tidy make;;

"linux")
  sudo add-apt-repository -y ppa:robert7/tidy-html5
  sudo apt-get update
  sudo apt-get install tidy
;;

"osx") brew install tidy-html5 ;;
esac

if [[ $ESY_BUILD == YES ]]; then
  npm --global install esy@0.3.x
else
  OPAM_RELEASES=https://github.com/ocaml/opam/releases/
  OPAM_VERSION=2.0.0
  OPAM_PKG=opam-${OPAM_VERSION}-x86_64

  case $TRAVIS_OS_NAME in
  "windows")
    wget ${OPAM_RELEASES}/download/${OPAM_VERSION}/${OPAM_PKG}-linux
    mv ${OPAM_PKG}-linux /usr/bin/opam
    chmod a+x /usr/bin/opam
  ;;

  "linux")
    wget ${OPAM_RELEASES}/download/${OPAM_VERSION}/${OPAM_PKG}-linux
    sudo mv ${OPAM_PKG}-linux /usr/local/bin/opam
    sudo chmod a+x /usr/local/bin/opam
  ;;

  "osx")
    brew install opam
  ;;
  esac

  opam init -y --compiler=$OCAML --disable-sandboxing
fi
