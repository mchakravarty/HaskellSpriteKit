{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards #-}

-- |
-- Module      : Graphics.SpriteKit.Geometry
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Geometry types and operations

module Graphics.SpriteKit.Geometry (
  Point(..), Size(..),
  pointZero, sizeZero, 
  
  -- * Marshalling functions
  pointToCGPoint, cgPointToPoint
) where
  
  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>"]


newtype Point = Point {x :: Float, y :: Float}

pointZero :: Point
pointZero = Point 0 0

newtype CGPoint = CGPoint (ForeignPtr CGPoint)
  deriving Typeable   -- needed for now until migrating to new TH

pointToCGPoint :: Point -> IO CGPoint
pointToCGPoint (Point {..})
  = do
    { pointPtr <- $(objc [x :> ''Float, y :> ''Float] $ Class ''CGPoint 
                      [cexp| ({ typename CGPoint *pnt = (CGPoint *) malloc(sizeof(CGPoint)); *pnt = CGPointMake(x, y); pnt }) |] )
    ; return $ CGPoint pointPtr
    }

cgPointToPoint :: CGPoint -> IO Point
cgPointToPoint (CGPoint pointPtr)
  = do
    { x <- peekElemOff pointPtr 0
    ; y <- peekElemOff pointPtr 1
    ; free pointPtr
    ; return $ Point x y
    }

newtype Size  = Size  {width :: Float, height :: Float}

sizeZero :: Size
sizeZero = Size 0 0

newtype CGSize = CGSize (ForeignPtr CGSize)
  deriving Typeable   -- needed for now until migrating to new TH

sizeToCGSize :: Size -> IO CGSize
sizeToCGSize (Size {..})
  = do
    { sizePtr <- $(objc [width :> ''Float, height :> ''Float] $ Class ''CGSize
                     [cexp| ({ typename CGSize *sz = (CGSize *) malloc(sizeof(CGSize)); *sz = CGSizeMake(width, height); sz }) |] )
    ; return $ CGSize sizePtr
    }

cgSizeToSize :: CGSize -> IO Size
cgSizeToSize (CGSize sizePtr)
  = do
    { width  <- peekElemOff sizePtr 0
    ; height <- peekElemOff sizePtr 1
    ; free sizePtr
    ; return $ Size width height
    }

