#!/bin/sh

#  CompileHaskellFiles.sh
#  HaskellSpriteKit
#
#  Created by Manuel M T Chakravarty on 18.09.16.
#  Copyright © 2016 Manuel M T Chakravarty.

# Set up variables for Haskell tools
source HaskellSpriteKit/HaskellConfigVars.sh

# Install dependencies for standalone setup
if [ $GHC_FRAMEWORK_EXISTS = "NO" ];
then
  $CABAL install --with-compiler=$GHC --with-hc-pkg=$GHC_PKG --with-haddock=$HADDOCK --only-dependencies HaskellSpriteKit/spritekit
  if [ $? != 0 ];
  then
    echo "cabal install --only-dependencies"
  fi
fi

# Generate Haddock docs in a separate cabal install run. (Otherwise, the Haddock invocation
# will make language-c-inline overwrite the generated ObjC files with somewhat different
# versions.)
#
# NB: Need to run Haddock first; otherwise, it will override the package configuration in the
#     package database with an incorrect one.
if [ $CONFIGURATION = "Release" ];
then
  CABAL_CMD="$CABAL install --builddir=$TARGET_TEMP_DIR/spritekit $CABAL_PREFIX --bindir=$TARGET_TEMP_DIR/spritekit-scratch --libdir=$TARGET_TEMP_DIR/spritekit-scratch --libexecdir=$TARGET_TEMP_DIR/spritekit-scratch --datadir=$TARGET_TEMP_DIR/spritekit-scratch $CABAL_DOCDIRS --with-compiler=$GHC --with-hc-pkg=$GHC_PKG --with-haddock=$HADDOCK $CABAL_OPTIONS --disable-library-profiling --force-reinstalls --enable-documentation HaskellSpriteKit/spritekit"
  echo $CABAL_CMD
  $CABAL_CMD
  if [ $? != 0 ];
  then
    echo "Haddock failed"
    exit 1
  fi
  #  rm -f $SRCROOT/HaskellSpriteKit/spritekit/Graphics/SpriteKit/*_objc.{h,m}
fi

# NB: We cannot allow profiling builds as they would need a second version of the Objective-C wrappers as well.
rm -rf $TARGET_TEMP_DIR/spritekit
CABAL_CMD="$CABAL install --builddir=$TARGET_TEMP_DIR/spritekit $CABAL_PREFIX $CABAL_DIRS --with-compiler=$GHC --with-hc-pkg=$GHC_PKG $CABAL_OPTIONS --force-reinstalls --disable-library-profiling --disable-documentation --ghc-options=$GHC_OPTIONS HaskellSpriteKit/spritekit"
echo $CABAL_CMD
$CABAL_CMD
if [ $? != 0 ];
then
  echo "cabal install failed"
  exit 1
fi

if [ $GHC_FRAMEWORK_EXISTS = "YES" ];
then
  LIBPATH=`$GHC_PKG --package-db=$GHCLIB/package.conf.d field $PKG library-dirs | cut -f 2 -d ' '`
else
  LIBPATH=`$GHC_PKG field $PKG library-dirs | cut -f 2 -d ' '`
fi

if [ $GHC_FRAMEWORK_EXISTS = "YES" ];
then

  # Relocatable dynamic library setup for Haskell for Mac builds (please don't change this)
  KEY=`$GHC_PKG --package-db=$GHCLIB/package.conf.d field $PKG key | cut -f 2 -d ' '`
  HSLIB=`$GHC_PKG --package-db=$GHCLIB/package.conf.d field $PKG hs-libraries | cut -f 2 -d ' '`
  NAME=lib${HSLIB}-ghc${GHC_VERSION}.dylib
  DIR=`basename $LIBPATH`
  echo "Fix install name and rpath: $DIR/$NAME (key: $KEY)"
  install_name_tool -id "@rpath/$DIR/$NAME" $LIBPATH/$NAME
  install_name_tool -add_rpath "@loader_path/.." $LIBPATH/$NAME
  # Get rid of absolute RPATHs to avoid hiding bugs in debug runs that affect the release.
  for path in `otool -l $LIBPATH/$NAME | grep path | grep BinaryFrameworks | cut -d ' ' -f 11`; do
    install_name_tool -delete_rpath $path $LIBPATH/$NAME
  done

fi

# Copy ObjC code generated from inline code (ignore empty files)
files=""
for file in `ls $SRCROOT/HaskellSpriteKit/spritekit/Graphics/SpriteKit/*_objc.{h,m} 2>/dev/null`; do
  if [ -s $file ]; then
    files="$files $file"
  fi
done
echo "Generated ObjC files: $files"
if [ -n "$files" ];
then
  mv -fv $files ${DERIVED_FILE_DIR}
fi
