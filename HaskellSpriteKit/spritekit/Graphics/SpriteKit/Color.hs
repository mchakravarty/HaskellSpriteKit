{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Color
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Colours

module Graphics.SpriteKit.Color (

  -- * Colour representation
  Color, 
  
  -- * Colours in the sRGBA colour space
  colorWithRGBA, rgbaOfColor,
  
  -- * Standard colours
  blackColor, blueColor, clearColor, cyanColor, darkGrayColor, grayColor, greenColor, lightGrayColor, magentaColor, 
  orangeColor, purpleColor, redColor, whiteColor, yellowColor,
  
  -- * Marshalling support (internal)
  SKColor(..),
  
  color_initialise
) where

  -- standard libraries
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- |Representation of colour values.
--
type Color = SKColor

-- We maintain colours as a reference to the native representation.
--
newtype SKColor = SKColor (ForeignPtr SKColor)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

-- |Create colour with the specified red, green, blue, and alpha channel values in the sRGB colour space.
--
colorWithRGBA :: Float -> Float -> Float -> Float -> Color
colorWithRGBA red green blue alpha
  = unsafePerformIO $(objc ['red :> ''Float, 'green :> ''Float, 'blue :> ''Float, 'alpha :> ''Float] $ Class ''SKColor <: 
                        [cexp| [SKColor colorWithRed:red green:green blue:blue alpha:alpha] |])

-- |Extract the red, green, blue, and alpha channel values of a colour in the sRGB colour space.
--
rgbaOfColor :: Color -> (Float, Float, Float, Float)
rgbaOfColor color
  = unsafePerformIO $ do
    -- FIXME: It would be more efficient to use 'getRed:green:blue:alpha:'
    { red   <- $(objc ['color :> Class ''SKColor] $ ''Float <: [cexp| color.redComponent |])
    ; green <- $(objc ['color :> Class ''SKColor] $ ''Float <: [cexp| color.greenComponent |])
    ; blue  <- $(objc ['color :> Class ''SKColor] $ ''Float <: [cexp| color.blueComponent |])
    ; alpha <- $(objc ['color :> Class ''SKColor] $ ''Float <: [cexp| color.alphaComponent |])
    ; return (red, green, blue, alpha)
    }

blackColor, blueColor, clearColor, cyanColor, darkGrayColor, grayColor, greenColor, lightGrayColor, magentaColor, 
  orangeColor, purpleColor, redColor, whiteColor, yellowColor :: Color
blackColor     = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor blackColor] |])
blueColor      = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor blueColor] |])
clearColor     = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor clearColor] |])
cyanColor      = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor cyanColor] |])
darkGrayColor  = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor darkGrayColor] |])
grayColor      = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor grayColor] |])
greenColor     = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor greenColor] |])
lightGrayColor = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor lightGrayColor] |])
magentaColor   = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor magentaColor] |])
orangeColor    = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor orangeColor] |])
purpleColor    = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor purpleColor] |])
redColor       = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor redColor] |])
whiteColor     = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor whiteColor] |])
yellowColor    = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor yellowColor] |])

objc_emit

color_initialise = objc_initialise
