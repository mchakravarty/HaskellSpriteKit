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

  -- ** SpriteKit texture representation
  Texture(..),
  
  -- ** Texture creation
  textureWithImageNamed, textureWithImage, textureWithRectInTexture,
  
  -- ** Texture properties
  textureSize, textureRect,

  -- ** Marshalling support
  SKTexture(..), 
  textureToSKTexture, textureToForeignPtr,
  
  texture_initialise
) where

  -- standard libraries
import Control.Applicative
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

  -- friends
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Image
import Graphics.SpriteKit.IO

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]

objc_struct_marshaller 'sizeToCGSize 'cgSizeToSize
objc_struct_marshaller 'rectToCGRect 'cgRectToRect

-- FIXME: for iOS, this needs to be UIImage — see 'Image.hs'
objc_interface [cunit|
typedef typename NSImage NSUIImage;
|]


-- |A SpriteKit texture object
--
newtype Texture = Texture SKTexture

-- 'SKTexture' objects are immutable; so, we represent them in Haskell as a foreign pointer to the
-- native representation.
--
newtype SKTexture = SKTexture (ForeignPtr SKTexture)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck


-- Texture creation from images
-- ----------------------------

-- |Create a texture from an image in the app bundle (either a file or an image in a texture atlas) or external file.
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
    ; Texture <$> 
        $(objc ['resolvedImageName :> ''String] $ Class ''SKTexture <: 
          [cexp| [SKTexture textureWithImageNamed:resolvedImageName] |])
    }

-- |Create a texture from the given image.
--
textureWithImage :: Image -> Texture
textureWithImage image
  = unsafePerformIO $ do
    { nsuiImage <- imageToNSUIImage image
    ; Texture <$> 
        $(objc ['nsuiImage :> Class ''NSUIImage] $ Class ''SKTexture <: 
          [cexp| [SKTexture textureWithImage:nsuiImage] |])
    }

-- |Create a texture from a subset of an existing texture.
--
-- The original and new texture share the same texture data. In further call to the same function, the rectangle is again
-- specified in terms of the *original* texture.
--
textureWithRectInTexture :: Rect -> Texture -> Texture
textureWithRectInTexture rect (Texture texture)
  = unsafePerformIO $
      Texture <$> 
        $(objc ['rect :> ''Rect, 'texture :> Class ''SKTexture] $ Class ''SKTexture <: 
          [cexp| [SKTexture textureWithRect:*rect inTexture:texture] |])

-- Currently unsupported texture creation:
-- * Texture creation from CIImages ('textureWithCGImage:')
-- * Texture creation by applying a Core Image filter ('textureByApplyingCIFilter:')
-- * Texture creation from raw pixel data ('textureWithData:size:', 'textureWithData:size:rowLength:alignment:',
--   'textureWithData:size:flipped')


-- Properties
-- ----------

-- |The size of the texture.
--
textureSize :: Texture -> Size
textureSize (Texture texture)
  = unsafePerformIO $(objc ['texture :> Class ''SKTexture] $ ''Size <:
                      [cexp| ({ 
                        typename CGSize *sz = (typename CGSize *) malloc(sizeof(CGSize)); 
                        *sz = [texture size]; 
                        sz; 
                      }) |] )

-- |The rectangle (in the unit coordinate space) that defines the portion of the texture used to render its image.
--
-- The default is (0,0) — (1,1), but it may be different for textures created with 'textureWithRectInTexture'.
--
textureRect :: Texture -> Rect
textureRect (Texture texture)
  = unsafePerformIO $(objc ['texture :> Class ''SKTexture] $ ''Rect <:
                      [cexp| ({ 
                        typename CGRect *rect = (typename CGRect *) malloc(sizeof(CGRect)); 
                        *rect = [texture textureRect]; 
                        rect; 
                      }) |] )

-- FIXME: Features not yet supported:
--   * Non-default filtering modes: 'filteringMode'
--   * 'usesMipmaps'
--   * preloading textures: 'preloadWithCompletionHandler:' and 'preloadTextures:withCompletionHandler:'
--   NB: The first two are read/write properties, but you'd expect them to be set on texture creation (except for
--       dynamic changes to optimise rendering speed). We could offer them as options on texture creation and/or
--       provide functions to set them, but those would have to return a copied texture object to be pure. (That is
--       probably still efficient as the image data ought to be shared.)
--
-- FIXME: Yosemite-only features not yet supported:
--   * 'textureByGeneratingNormalMap' and 'textureByGeneratingNormalMapWithSmoothness:contrast:'
--   * 'textureVectorNoiseWithSmoothness:size:' and 'textureNoiseWithSmoothness:size:grayscale:'


-- Marshalling
-- -----------

textureToSKTexture :: Texture -> SKTexture
textureToSKTexture (Texture skTexture) = skTexture

textureToForeignPtr :: Texture -> IO (ForeignPtr SKTexture)
textureToForeignPtr (Texture (SKTexture tex)) = return tex

objc_emit

texture_initialise = objc_initialise
