{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

-- |
-- Module      : Graphics.SpriteKit.Texture
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Textures

module Graphics.SpriteKit.Texture (
  Texture,
  textureWithImageName, textureSize
) where

  -- standard libraries
import System.IO.Unsafe (unsafePerformIO)

  -- friends
import Graphics.SpriteKit.Geometry

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>"]


-- |A SpriteKit texture object
--
type Texture = SKTexture

-- 'SKTexture' objects are immutable; so, we represent them in Haskell as a foreign pointer to the
-- native representation.
--
newtype SKTexture = SKTexture (ForeignPtr SKTexture)
  deriving Typeable   -- needed for now until migrating to new TH

textureWithImageNamed :: FilePath -> Texture
textureWithImageNamed fname
  = $(objc [fname :> ''String] $ Class ''SKTexture <: [cexp| [SKTexture textureWithImageName:fname] |])

textureSize :: Texture -> Size
textureSize texture
  = unsafePerformIO $(objc [texture :> Class ''SKTexture] $ ''Size <: [cexp| [texture size] |])
