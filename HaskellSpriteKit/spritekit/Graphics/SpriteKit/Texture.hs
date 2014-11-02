{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ForeignFunctionInterface #-}

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

  -- * SpriteKit texture representation
  Texture,
  
  -- * Texture creation
  -- textureWithImageFile, textureWithImageNamed, 
  textureWithImageNamed, 
  
  -- * Texture properties
  textureSize,

  -- * Marshalling support
  SKTexture(..),
  
  texture_initialise
) where

  -- standard libraries
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

  -- friends
import Graphics.SpriteKit.Geometry

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]

objc_marshaller 'sizeToCGSize 'cgSizeToSize


-- |A SpriteKit texture object
--
type Texture = SKTexture

-- 'SKTexture' objects are immutable; so, we represent them in Haskell as a foreign pointer to the
-- native representation.
--
newtype SKTexture = SKTexture (ForeignPtr SKTexture)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck


{-
-- |Create a texture from an arbitrary image file.
--
-- A placeholder image is used if the file cannot be loaded.
--
textureWithImageFile :: FilePath -> Texture
textureWithImageFile fname
  = unsafePerformIO 
      $(objc ['fname :> ''String] $ Class ''SKTexture <: 
        [cexp| ({
          typename NSImage *image = [[NSImage alloc] initWithContentsOfFile:fname];
          (image) ? [SKTexture textureWithImage:image] : [SKTexture textureWithImageNamed:fname];
        }) |])
-}

-- |Create a texture an image in the app bundle (either a file or an image in a texture atlas).
--
-- A placeholder image is used if the specified image cannot be loaded.
--
-- NB: This function is not useful for interactive development. Use 'spriteWithImageFile' instead.
--
textureWithImageNamed :: FilePath -> Texture
textureWithImageNamed imageName
  = unsafePerformIO $(objc ['imageName :> ''String] $ Class ''SKTexture <: [cexp| [SKTexture textureWithImageNamed:imageName] |])

-- |The size of the texture.
--
textureSize :: Texture -> Size
textureSize texture
  = unsafePerformIO $(objc ['texture :> Class ''SKTexture] $ ''Size <:
                      [cexp| ({ 
                        typename CGSize *sz = (typename CGSize *) malloc(sizeof(CGSize)); 
                        *sz = [texture size]; 
                        sz; 
                      }) |] )

objc_emit

texture_initialise = objc_initialise
