{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

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
  Color, colorWithRGBA
) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>"]


type Color = NSColor

-- We maintain colours as a reference to the native representation.
--
newtype NSColor = NSColor (ForeignPtr NSColor)
  deriving Typeable   -- needed for now until migrating to new TH

colorWithRGBA :: Float -> Float -> Float -> Float -> Color
colorWithRGBA red green blue alpha
  = $(objc [red :> ''Float, green :> ''Float, blue :> ''Float, alpha :> ''Float] $ Class ''NSColor <: 
        [cexp| [NSColor colorWithRed:red green:green blue:blue alpha:alpha] |])

