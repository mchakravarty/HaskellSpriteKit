{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}

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
  pointToCGPoint, cgPointToPoint,
  sizeToCGSize, cgSizeToSize,
  
  geometry_initialise
) where
  
  -- standard libraries
import Data.Typeable
import Foreign

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


data Point = Point {pointX :: Float, pointY :: Float}

pointZero :: Point
pointZero = Point 0 0

newtype CGPoint = CGPoint (ForeignPtr CGPoint)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

pointToCGPoint :: Point -> IO CGPoint
pointToCGPoint (Point {..})
  = $(objc ['pointX :> ''Float, 'pointY :> ''Float] $ Class ''CGPoint <:
       [cexp| ({ 
         typename CGPoint *pnt = (typename CGPoint *) malloc(sizeof(CGPoint)); 
         *pnt = CGPointMake(pointX, pointY); 
         pnt; 
       }) |] )

cgPointToPoint :: CGPoint -> IO Point
cgPointToPoint (CGPoint pointPtr)
  = withForeignPtr pointPtr $ \pointPtr -> do
    { x <- peekElemOff (castPtr pointPtr :: Ptr Float) 0
    ; y <- peekElemOff (castPtr pointPtr :: Ptr Float) 1
    -- ; free pointPtr
    ; return $ Point x y
    }

data Size = Size {sizeWidth :: Float, sizeHeight :: Float}
  deriving Typeable

sizeZero :: Size
sizeZero = Size 0 0

newtype CGSize = CGSize (ForeignPtr CGSize)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

sizeToCGSize :: Size -> IO CGSize
sizeToCGSize (Size {..})
  = $(objc ['sizeWidth :> ''Float, 'sizeHeight :> ''Float] $ Class ''CGSize <:
        [cexp| ({ 
          typename CGSize *sz = (typename CGSize *) malloc(sizeof(CGSize)); 
          *sz = CGSizeMake(sizeWidth, sizeHeight); 
          sz; 
        }) |] )

cgSizeToSize :: CGSize -> IO Size
cgSizeToSize (CGSize sizePtr)
  = withForeignPtr sizePtr $ \sizePtr -> do
    { width  <- peekElemOff (castPtr sizePtr :: Ptr Float) 0
    ; height <- peekElemOff (castPtr sizePtr :: Ptr Float) 1
    -- ; free sizePtr
    ; return $ Size width height
    }

objc_emit

geometry_initialise = objc_initialise
