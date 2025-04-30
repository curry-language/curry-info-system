#!/bin/sh
# Download the cache of CurryInfo from
# https://cpm.curry-lang.org/curry-info/HTML/CURRYINFOCACHE.tgz
# and install it locally in ~/.curry_info_cache.
# An existing local CurryInfo cache is saved before installation.

set -e

TARURL=https://cpm.curry-lang.org/curry-info/HTML/CURRYINFOCACHE.tgz
CACHEDIR=.curry_info_cache

cd $HOME
if [ -d $CACHEDIR ] ; then
  SAVEDIR="$CACHEDIR"_`date -r $CACHEDIR +%Y_%m_%d_%H_%M_%S`
  rm -rf $SAVEDIR
  mv $CACHEDIR $SAVEDIR
  echo "Contents of ~/$CACHEDIR moved to ~/$SAVEDIR"
fi

mkdir $CACHEDIR
cd $CACHEDIR
echo "Downloading $TARURL..."
curl -sSL $TARURL | tar xz
echo "Downloaded contents installed in ~/$CACHEDIR"
touch $HOME/$CACHEDIR
