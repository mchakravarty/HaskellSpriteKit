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

  -- * Basic temporal definition
  TimeInterval,
  
  -- * Basic geometry definitions
  GFloat, Point(..), Size(..), Vector(..),
  pointZero, sizeZero, 
  
  -- * Marshalling functions (internal)
  pointToCGPoint, cgPointToPoint,
  sizeToCGSize, cgSizeToSize,
  vectorToCGVector, cgVectorToVector,
  
  geometry_initialise
) where
  
  -- standard libraries
import Data.Typeable
import Foreign

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- Temporal definitions
-- --------------------

type TimeInterval = Double


-- Basic geometry definitions
-- --------------------------

-- |Graphics float (which is a 'Float' or 'Double' depending on whether we are on a 32 or 64 bit architecture)
--
type GFloat = Double      -- ^FIXME: need to be set in dependence on the definition of 'CGFloat' resp 'CGFLOAT_IS_DOUBLE'

-- |Point in a two-dimensional coordinate system.
--
data Point = Point {pointX :: GFloat, pointY :: GFloat}

-- |Point at (0, 0).
--
pointZero :: Point
pointZero = Point 0 0

-- |Size of a two-dimensional geometry entity.
--
data Size = Size {sizeWidth :: GFloat, sizeHeight :: GFloat}
  deriving Typeable   -- needed for now until migrating to new TH

sizeZero :: Size
sizeZero = Size 0 0

-- |Two-dimensional vector.
--
data Vector = Vector {vectorDx :: GFloat, vectorDy :: GFloat}
  deriving Typeable   -- needed for now until migrating to new TH


-- Marshalling support
-- -------------------

newtype CGPoint = CGPoint (ForeignPtr CGPoint)
  deriving Typeable   -- needed for now until migrating to new TH
  -- FIXME: CGPoint, CGSize & CGVector need free() as a finaliser not '-release'.
  --        How should language-c-inline distinguish? Check whether it is an object?

objc_typecheck

pointToCGPoint :: Point -> IO CGPoint
pointToCGPoint (Point {..})
  -- FIXME: language-c-inline needs to look through type synonyms
  -- = $(objc ['pointX :> ''GFloat, 'pointY :> ''GFloat] $ Class ''CGPoint <:
  = $(objc ['pointX :> ''Double, 'pointY :> ''Double] $ Class ''CGPoint <:
       [cexp| ({ 
         typename CGPoint *pnt = (typename CGPoint *) malloc(sizeof(CGPoint)); 
         *pnt = CGPointMake(pointX, pointY); 
         pnt; 
       }) |] )

cgPointToPoint :: CGPoint -> IO Point
cgPointToPoint (CGPoint pointPtr)
  = withForeignPtr pointPtr $ \pointPtr -> do
    { x <- peekElemOff (castPtr pointPtr :: Ptr GFloat) 0
    ; y <- peekElemOff (castPtr pointPtr :: Ptr GFloat) 1
    ; return $ Point x y
    }

newtype CGSize = CGSize (ForeignPtr CGSize)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

sizeToCGSize :: Size -> IO CGSize
sizeToCGSize (Size {..})
  -- FIXME: language-c-inline needs to look through type synonyms
  -- = $(objc ['sizeWidth :> ''GFloat, 'sizeHeight :> ''GFloat] $ Class ''CGSize <:
  = $(objc ['sizeWidth :> ''Double, 'sizeHeight :> ''Double] $ Class ''CGSize <:
        [cexp| ({ 
          typename CGSize *sz = (typename CGSize *) malloc(sizeof(CGSize)); 
          *sz = CGSizeMake(sizeWidth, sizeHeight); 
          sz; 
        }) |] )

cgSizeToSize :: CGSize -> IO Size
cgSizeToSize (CGSize sizePtr)
  = withForeignPtr sizePtr $ \sizePtr -> do
    { width  <- peekElemOff (castPtr sizePtr :: Ptr GFloat) 0
    ; height <- peekElemOff (castPtr sizePtr :: Ptr GFloat) 1
    ; return $ Size width height
    }

newtype CGVector = CGVector (ForeignPtr CGVector)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

vectorToCGVector :: Vector -> IO CGVector
vectorToCGVector (Vector {..})
  -- FIXME: language-c-inline needs to look through type synonyms
  -- = $(objc ['vectorDx :> ''GFloat, 'vectorDy :> ''GFloat] $ Class ''CGVector <:
  = $(objc ['vectorDx :> ''Double, 'vectorDy :> ''Double] $ Class ''CGVector <:
        [cexp| ({ 
          typename CGVector *vec = (typename CGVector *) malloc(sizeof(CGVector)); 
          *vec = CGVectorMake(vectorDx, vectorDy); 
          vec; 
        }) |] )

cgVectorToVector :: CGVector -> IO Vector
cgVectorToVector (CGVector vectorPtr)
  = withForeignPtr vectorPtr $ \vectorPtr -> do
    { vectorDx <- peekElemOff (castPtr vectorPtr :: Ptr GFloat) 0
    ; vectorDy <- peekElemOff (castPtr vectorPtr :: Ptr GFloat) 1
    ; return $ Vector vectorDx vectorDy
    }

objc_emit

geometry_initialise = objc_initialise
