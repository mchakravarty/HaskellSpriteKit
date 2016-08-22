# Haskell binding to Apple's SpriteKit framework

Open source under BSD3 license. Contributions under the same license are most welcome.

The build is via Xcode and currently expects to be embedded into a larger project. A binary version of HaskellSpriteKit is being distributed embedded in [Haskell for Mac](http://haskellformac.com).

This has currently only been tested on macOS. The main obstacle to using it on iOS is lack of support for Template Haskell (and hence, `language-c-inline`) by GHC for iOS.


## BIG CAVEAT

The current build system expects this project to be embedded into the Haskell for Mac Xcode project. In particular, it assumes `GHC.framework` to be around and to be used to compile all Haskell code. This will be generalised, but for the moment, you'll have to tweak some paths on the build scripts and in some Xcode build settings.

The easiest way to use HaskellSpriteKit at the moment is to use the version that comes with [Haskell for Mac](http://haskellformac.com).


## SpriteKit features that are not yet supported

* Only sprite, shape, and label nodes are currently supported.
* The various search routines are not supported.
* Joints, constraints, and fields are not supported.
* Most features introduced with macOS 10.10 and later are not supported.
* No macOS 10.12 features are supported.