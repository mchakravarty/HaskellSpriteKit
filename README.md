# Haskell binding to Apple's SpriteKit framework

Open source under BSD3 license. Contributions under the same license are most welcome.

The build is via Xcode and currently expects to be embedded into a larger project. A binary version of HaskellSpriteKit with [Haskell for Mac](http://haskellformac.com), which also provides the best Haskell SpriteKit development experience.

This code has currently only been tested on macOS. The main obstacle to using it on iOS is lack of support for Template Haskell (and hence, `language-c-inline`) by GHC for iOS.


## BIG CAVEAT

The current build system expects this project to be embedded into the Haskell for Mac Xcode project. In particular, it assumes `GHC.framework` to be around and to be used to compile all Haskell code. This will be generalised, but for the moment, you'll have to tweak some paths on the build scripts and in some Xcode build settings.

The easiest way to use HaskellSpriteKit at the moment is to use the version that comes with [Haskell for Mac](http://haskellformac.com).

Moreover, this project needs its own suite of unit tests.


## SpriteKit features that are not yet supported

* Only sprite, shape, and label nodes are currently supported.
* The various search routines are not supported.
* Joints, constraints, and fields are not supported.
* Most features introduced with macOS 10.10 and later are not supported.
* No macOS 10.12 features are supported.

Most of the missing features can be supported quite easily by simply following the same approach as used in the existing code.


## An example game: Lazy Lambda

![Lazy Lambda Loop](https://raw.githubusercontent.com/mchakravarty/lazy-lambda/master/images/LazyLambdaLoop.gif)

As an example of a very simple, but fairly complete game in Haskell SpriteKit, have a look at Lazy Lambda, a Flappy Bird clone:

> https://github.com/mchakravarty/lazy-lambda

For an explanation of the main concepts of the Haskell SpriteKit binding including live coding of Lazy Lambda, watch the talk [Playing with Graphics and Animations in Haskell](https://speakerdeck.com/mchakravarty/playing-with-graphics-and-animations-in-haskell) (includes video and slides).