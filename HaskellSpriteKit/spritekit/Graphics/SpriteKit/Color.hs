{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ForeignFunctionInterface, BangPatterns #-}

-- |
-- Module      : Graphics.SpriteKit.Color
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Colors

module Graphics.SpriteKit.Color (

  -- * Colour representation
  Color(..), 
  
  -- * Colours in the sRGBA colour space
  colorWithRGBA, rgbaOfColor,
  
  -- * Standard colours
  blackColor, blueColor, clearColor, cyanColor, darkGrayColor, grayColor, greenColor, lightGrayColor, magentaColor, 
  orangeColor, purpleColor, redColor, whiteColor, yellowColor,
  
  -- * Marshalling support (internal)
  SKColor(..), colorToSKColor, colorToForeignPtr,
  
  color_initialise
) where

  -- standard libraries
import Control.Applicative
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- |Representation of color values.
--
newtype Color = Color SKColor

-- We maintain colors as a reference to the native representation.
--
newtype SKColor = SKColor (ForeignPtr SKColor)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

-- |Create color with the specified red, green, blue, and alpha channel values in the sRGB color space.
--
colorWithRGBA :: Float -> Float -> Float -> Float -> Color
colorWithRGBA !red !green !blue !alpha
  = unsafePerformIO $
      Color <$> $(objc ['red :> ''Float, 'green :> ''Float, 'blue :> ''Float, 'alpha :> ''Float] $ Class ''SKColor <: 
                   [cexp| [SKColor colorWithRed:red green:green blue:blue alpha:alpha] |])

-- |Extract the red, green, blue, and alpha channel values of a colour in the sRGB colour space. 
--
-- Returns 'Nothing' if the color is in a different color space.
--
rgbaOfColor :: Color -> Maybe (Float, Float, Float, Float)
rgbaOfColor (Color color)
  = unsafePerformIO $ do
    -- FIXME: It would be more efficient to use 'getRed:green:blue:alpha:'
    { valid <- $(objc ['color :> Class ''SKColor] $ ''Bool <: 
                  [cexp| color.colorSpaceName == NSCalibratedRGBColorSpace || 
                         color.colorSpaceName == NSDeviceRGBColorSpace |] )
    ; if valid 
        then do
        { red   <- $(objc ['color :> Class ''SKColor] $ ''Float <: [cexp| color.redComponent |])
        ; green <- $(objc ['color :> Class ''SKColor] $ ''Float <: [cexp| color.greenComponent |])
        ; blue  <- $(objc ['color :> Class ''SKColor] $ ''Float <: [cexp| color.blueComponent |])
        ; alpha <- $(objc ['color :> Class ''SKColor] $ ''Float <: [cexp| color.alphaComponent |])
        ; return $ Just (red, green, blue, alpha)
        }
        else return Nothing
    }

blackColor, blueColor, clearColor, cyanColor, darkGrayColor, grayColor, greenColor, lightGrayColor, magentaColor, 
  orangeColor, purpleColor, redColor, whiteColor, yellowColor :: Color
blackColor     = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor blackColor] |])
blueColor      = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor blueColor] |])
clearColor     = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor clearColor] |])
cyanColor      = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor cyanColor] |])
darkGrayColor  = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor darkGrayColor] |])
grayColor      = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor grayColor] |])
greenColor     = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor greenColor] |])
lightGrayColor = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor lightGrayColor] |])
magentaColor   = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor magentaColor] |])
orangeColor    = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor orangeColor] |])
purpleColor    = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor purpleColor] |])
redColor       = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor redColor] |])
whiteColor     = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor whiteColor] |])
yellowColor    = unsafePerformIO $ Color <$> $(objc [] $ Class ''SKColor <: [cexp| [SKColor yellowColor] |])

-- Marshalling
-- -----------

colorToSKColor :: Color -> SKColor
colorToSKColor (Color skColor) = skColor

colorToForeignPtr :: Color -> IO (ForeignPtr SKColor)
colorToForeignPtr (Color (SKColor col)) = return col

objc_emit

color_initialise = objc_initialise
