{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Texture
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Textures

module Graphics.SpriteKit.Texture (

  -- * SpriteKit texture representation
  Texture,
  
  -- * Texture creation
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
import Graphics.SpriteKit.IO

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


-- |Create a texture an image in the app bundle (either a file or an image in a texture atlas).
--
-- A placeholder image is used if the specified image cannot be loaded.
--
-- In case of a relative name, the look up behaviour varies between a compiled application bundle and running in the
-- interpreter. In case of a compiled application bundle, the standard SpriteKit bundle behaviour applies. In case of
-- executing in the interpreter, we first look for assets relative to the current working directory.
--
textureWithImageNamed :: FilePath -> Texture
textureWithImageNamed imageName
  = unsafePerformIO $ do
    { resolvedImageName <- lookupFileName imageName
    ; $(objc ['resolvedImageName :> ''String] $ Class ''SKTexture <: 
        [cexp| [SKTexture textureWithImageNamed:resolvedImageName] |])
    }

-- Properties
-- ----------

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

-- FIXME: Features not yet supported:
--   * Texture from NSImage ('textureWithImage:') and CIImage ('textureWithCGImage:')
--   * Creation of derivative textures: 'textureWithRect:inTexture:' and 'textureByApplyingCIFilter:'
--   * Texture creation from raw pixel data: three methods
--   * Non-default filtering modes: 'filteringMode'
--   * Querying 'textureRect'
--   * 'usesMipmaps'
--   * preloading textures: 'preloadWithCompletionHandler:' and 'preloadTextures:withCompletionHandler:'
--
-- FIXME: Yosemite-only features not yet supported:
--   * 'textureByGeneratingNormalMap' and 'textureByGeneratingNormalMapWithSmoothness:contrast:'
--   * 'textureVectorNoiseWithSmoothness:size:' and 'textureNoiseWithSmoothness:size:grayscale:'


objc_emit

texture_initialise = objc_initialise
