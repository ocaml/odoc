#!/bin/bash

readonly MIRROR=http://mirrors.kernel.org/ubuntu/pool/universe/t/tidy-html5
readonly TIDY_VERSION=5.2.0
readonly TIDY_PKG=tidy_${TIDY_VERSION}-2_amd64.deb
readonly TIDY_URL=${MIRROR}/${TIDY_PKG}

echo "Downloading from ${TIDY_URL}..."
wget -q $TIDY_URL

# NOTE(@ostera): This checksum comes from
# https://packages.ubuntu.com/bionic/amd64/tidy/download
echo "Verifying checksum..."
echo "06fda2013e8edb31fbc37fb2bb407e5c  ${TIDY_PKG}" > ${TIDY_PKG}.md5
md5sum -c ${TIDY_PKG}.md5
if [ $? -gt 0 ]; then
  echo ">> Error verifying integrity of deb package"
  exit 100
fi

echo "Installing..."
apt install ./${TIDY_PKG}
echo "Done!"
