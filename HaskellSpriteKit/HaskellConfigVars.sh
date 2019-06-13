#!/bin/sh

#  HaskellConfigVars.sh
#  HaskellSpriteKit
#
#  Created by Manuel M T Chakravarty on 21.09.16.
#  Copyright © [2016..2019] Manuel M T Chakravarty.

# Set up variables for Haskell tools

if [ -d $SOURCE_ROOT/../BinaryFrameworks/$CONFIGURATION/GHC.Framework ];
then
  GHC_FRAMEWORK_EXISTS=YES
else
  GHC_FRAMEWORK_EXISTS=NO
fi

PKG=spritekit-${CURRENT_PROJECT_VERSION}

if [ $GHC_FRAMEWORK_EXISTS = "YES" ];
then
  # Compiled with GHC.framework (please don't change this)

  # We need to standardise these paths as they will end up in Haskell package config files.
  GHC_CURRENT_RAW=$SOURCE_ROOT/../BinaryFrameworks/$CONFIGURATION/GHC.Framework/Versions/Current
  STANDARDIZE=$GHC_CURRENT_RAW/Executables/StandardizePath.buildtime
  GHC_CURRENT="`$STANDARDIZE $GHC_CURRENT_RAW`"
  GHC=$GHC_CURRENT/usr/bin/ghc
  GHC_PKG=$GHC_CURRENT/usr/bin/ghc-pkg
  HADDOCK=$GHC_CURRENT/usr/bin/haddock
  CABAL=/Library/Haskell/bin/cabal
  GHC_VERSION=`$GHC --numeric-version`

  echo "Building with GHC.framework: GHC_CURRENT = $GHC_CURRENT"

  GHCLIB=$GHC_CURRENT/usr/lib/ghc-${GHC_VERSION}
  GHCSHARE=$GHC_CURRENT/usr/share
  GHCDOC=$GHCSHARE/doc/ghc-${GHC_VERSION}

  GHC_OPTIONS="-optl-Wl,-headerpad_max_install_names"
  CABAL_PREFIX="--prefix=$GHCLIB"
  CABAL_DIRS="--bindir=$GHCLIB/bin --libdir=$GHCLIB --dynlibdir=$GHCLIB/spritekit-$CURRENT_PROJECT_VERSION --libexecdir=$GHCLIB/libexec --datadir=$GHCSHARE"
  CABAL_DOCDIRS="--docdir=$GHCDOC --htmldir=$GHCDOC/html/libraries/$PKG --haddockdir=$GHCDOC/html/libraries/$PKG"
  CABAL_OPTIONS="--package-db=clear --package-db=global --package-db=$GHCLIB/package.conf.d --disable-library-vanilla --ghc-option=-haddock"

else
  # Standalone setup (feel free to improve)

  GHC="`which ghc`"
  if [ -z "$GHC" ];
  then
    echo "Could not find GHC on PATH = $PATH"
    exit 1
  else
    echo "Building with standalone GHC"
  fi
  GHC_PKG="`which ghc-pkg`"
  CABAL="`which cabal`"
  HADDOCK="`which haddock`"
  GHC_VERSION=`$GHC --numeric-version`
  GHCLIB="`$GHC --print-libdir`"
  GHCSHARE=$GHCLIB/../../share
  GHCDOC=$GHCSHARE/doc/ghc

  GHC_OPTIONS=""
  CABAL_PREFIX=""
  CABAL_DIRS=""
  CABAL_DOCDIRS=""
  CABAL_OPTIONS=""

fi

# Change Haskell SpriteKit version in the Build Settings ("Current Project Version")
echo "GHC version = $GHC_VERSION; Haskell SpriteKit version = $CURRENT_PROJECT_VERSION"
echo "GHC binary = $GHC"
echo "NB: For now, change Haskell SpriteKit version in the Build Settings ('Current Project Version')"
