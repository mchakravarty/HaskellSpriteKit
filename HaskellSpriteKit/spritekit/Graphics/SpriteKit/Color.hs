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
  Color, colorWithRGBA,
  whiteColor,

  -- * Marshalling support
  SKColor(..)
) where

  -- standard libraries
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


type Color = SKColor

-- We maintain colours as a reference to the native representation.
--
newtype SKColor = SKColor (ForeignPtr SKColor)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

colorWithRGBA :: Float -> Float -> Float -> Float -> Color
colorWithRGBA red green blue alpha
  = unsafePerformIO $(objc ['red :> ''Float, 'green :> ''Float, 'blue :> ''Float, 'alpha :> ''Float] $ Class ''SKColor <: 
                        [cexp| [SKColor colorWithRed:red green:green blue:blue alpha:alpha] |])

whiteColor :: Color
whiteColor = unsafePerformIO $(objc [] $ Class ''SKColor <: [cexp| [SKColor whiteColor] |])

objc_interface [cunit|
  void Color_initialise(void);
|]

objc_emit

foreign export ccall "Color_initialise" objc_initialise :: IO ()
