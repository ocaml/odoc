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
esac

# Install esy or opam
if [[ $ESY_BUILD == YES ]]; then
  npm --global install esy@0.5.x
else
  OPAM_RELEASES=https://github.com/ocaml/opam/releases/
  OPAM_VERSION=2.0.5

  case $TRAVIS_OS_NAME in
  "linux") OPAM_OS=linux;;
    "osx") OPAM_OS=macos;;
        *) echo Unsupported system $TRAVIS_OS_NAME; exit 1;;
  esac

  OPAM_PKG=opam-${OPAM_VERSION}-x86_64-${OPAM_OS}

  wget ${OPAM_RELEASES}/download/${OPAM_VERSION}/${OPAM_PKG}
  sudo mv ${OPAM_PKG} /usr/local/bin/opam
  sudo chmod a+x /usr/local/bin/opam

  opam init -y --bare --disable-sandboxing

  if [ ! -d _opam/bin ]
  then
      rm -rf _opam
      opam switch create . $OCAML $REPOSITORIES --no-install
  fi
fi
