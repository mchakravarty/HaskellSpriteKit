{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, FlexibleInstances, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Image
-- Copyright   : [2015] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Geometry types and operations

module Graphics.SpriteKit.Image (

  -- ** Images
  Image, IsImage(..),

  -- ** Marshalling functions (internal)
  NSUIImage(..), imageToNSUIImage, imageToForeignPtr,

  image_initialise
) where

  -- standard libraries
import Data.ByteString.Lazy   (toStrict)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Typeable
import Foreign                hiding (void)

  -- JuicyPixels
import qualified Codec.Picture as JP

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "GHC/HsFFI.h"]


-- Images
-- ------

-- Generic image type that can represent Cocoa (Touch) and JuicyPixel images types.
--
data Image = ForeignImage NSUIImage
           | JuicyImage   JP.DynamicImage

-- Generic image inlet.
--
class IsImage img where
  toImage :: img -> Image

instance IsImage Image where
  toImage = id

instance IsImage JP.DynamicImage where
  toImage = JuicyImage

instance IsImage (JP.Image JP.Pixel8)	where
  toImage = JuicyImage . JP.ImageY8 

instance IsImage (JP.Image JP.Pixel16)	where
  toImage = JuicyImage . JP.ImageY16 
  
instance IsImage (JP.Image JP.PixelF)	where
  toImage = JuicyImage . JP.ImageYF 
  
instance IsImage (JP.Image JP.PixelYA8) where
  toImage = JuicyImage . JP.ImageYA8 
  
instance IsImage (JP.Image JP.PixelYA16) where
  toImage = JuicyImage . JP.ImageYA16 
  
instance IsImage (JP.Image JP.PixelRGB8) where
  toImage = JuicyImage . JP.ImageRGB8 
  
instance IsImage (JP.Image JP.PixelRGB16) where
  toImage = JuicyImage . JP.ImageRGB16 
  
instance IsImage (JP.Image JP.PixelRGBF) where
  toImage = JuicyImage . JP.ImageRGBF 
  
instance IsImage (JP.Image JP.PixelRGBA8) where
  toImage = JuicyImage . JP.ImageRGBA8 
  
instance IsImage (JP.Image JP.PixelRGBA16) where
  toImage = JuicyImage . JP.ImageRGBA16 
  
instance IsImage (JP.Image JP.PixelYCbCr8) where
  toImage = JuicyImage . JP.ImageYCbCr8 
  
instance IsImage (JP.Image JP.PixelCMYK8) where
  toImage = JuicyImage . JP.ImageCMYK8 
  
instance IsImage (JP.Image JP.PixelCMYK16) where
  toImage = JuicyImage . JP.ImageCMYK16 


-- Marshalling support
-- -------------------

newtype NSUIImage = NSUIImage (ForeignPtr NSUIImage)
  deriving Typeable   -- needed for now until migrating to new TH

-- FIXME: for iOS, this needs to be UIImage
objc_interface [cunit|

typedef typename NSImage NSUIImage;

|]

imageToNSUIImage :: IsImage img => img -> IO NSUIImage
imageToNSUIImage = toForeignPtr . toImage
  where
    toForeignPtr :: Image -> IO NSUIImage
    toForeignPtr (ForeignImage img) = return img
    toForeignPtr (JuicyImage   img) = dynamicImageToForeignPtr img

imageToForeignPtr :: IsImage img => img -> IO (ForeignPtr NSUIImage)
imageToForeignPtr img = do { NSUIImage nsuiImg <- imageToNSUIImage img; return nsuiImg}

-- FIXME: This works for OS X, but on iOS .tiff doesn't seem to be supported. Need to go through PNG or JPEG depending on the 
--        pixel format. Also need to use class name UIImage for iOS.
dynamicImageToForeignPtr :: JP.DynamicImage -> IO NSUIImage
dynamicImageToForeignPtr (JP.ImageY8 img)     = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageY16 img)    = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageYF _img)    = cannotConvert
dynamicImageToForeignPtr (JP.ImageYA8 img)    = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageYA16 img)   = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageRGB8 img)   = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageRGB16 img)  = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageRGBF _img)  = cannotConvert
dynamicImageToForeignPtr (JP.ImageRGBA8 img)  = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageRGBA16 img) = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageYCbCr8 img) = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageCMYK8 img)  = convertViaTiff img
dynamicImageToForeignPtr (JP.ImageCMYK16 img) = convertViaTiff img

cannotConvert = $(objc [] $ Class ''NSUIImage <: [cexp| [NSImage imageNamed: @"NSImageNameCaution"] |])
        -- FIXME: need a better image

convertViaTiff img 
  = unsafeUseAsCStringLen (toStrict (JP.encodeTiff img)) $ \(ptr, len) -> 
      $(objc ['ptr :> [t| Ptr CChar |], 'len :> ''Int] $ Class ''NSUIImage <:
        [cexp| [[NSImage alloc] initWithData:[NSData dataWithBytes:ptr length:len]] |])

objc_emit

image_initialise = objc_initialise
