#!/bin/bash

sudo add-apt-repository -y ppa:robert7/tidy-html5
sudo apt-get update
sudo apt-get install tidy

if [[ $ESY_BUILD == YES ]]; then
  npm --global install esy@0.3.x
else
  wget https://github.com/ocaml/opam/releases/download/2.0.0/opam-2.0.0-x86_64-linux
  sudo mv opam-2.0.0-x86_64-linux /usr/local/bin/opam
  sudo chmod a+x /usr/local/bin/opam

  opam init -y --compiler=$OCAML --disable-sandboxing
fi
