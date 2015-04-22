{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Path
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Geometry types and operations

module Graphics.SpriteKit.Path (

  -- * Core Graphics path representation
  Path, PathElement(..),

  -- * Marshalling functions (internal)
  CGPath(..), pathToCGPath, cgPathToPath,
  
  path_initialise
) where

  -- standard libraries
import Data.Typeable
import Foreign          hiding (void)
import System.IO.Unsafe (unsafePerformIO)

  -- friends
import Graphics.SpriteKit.Geometry

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- Graphics paths
-- --------------

-- |Description of a graphics path.
--
type Path = [PathElement]

-- |Elementary components of a graphics path.
--
-- The constructors 'AddLineToPoint', 'AddQuadCurveToPoint', 'AddCurveToPoint' may not start a graphics path.
--
data PathElement = MoveToPoint         !Point                -- ^Starts a new subpath at the given point.
                 | AddLineToPoint      !Point                -- ^Adds a line from the current to the given point.
                 | AddQuadCurveToPoint !Point !Point         -- ^Adds a quadratic curve with control and destination point.
                 | AddCurveToPoint     !Point !Point !Point  -- ^Adds a cubic curve with two control and one destination point.
                 | CloseSubpath                              -- ^Closes and completes the current subpath.
-- FIXME: we might to add field names (esp useful for the quadratic and cubic curves)

-- Marshalling support
-- -------------------

objc_interface [cunit|

typedef struct CGPath CGPath;
typedef struct CGPath CGMutablePath;

|]

-- objc_marshaller 'pointToCGPoint 'cgPointToPoint

newtype CGPath = CGPath (ForeignPtr CGPath)
  deriving Typeable   -- needed for now until migrating to new TH
newtype CGMutablePath = CGMutablePath (ForeignPtr CGMutablePath)
  deriving Typeable   -- needed for now until migrating to new TH
  -- FIXME: Use free() as a finaliser or '-release', or a 'CGRelease'?
  --        How should language-c-inline distinguish? Check whether it is an object?

objc_typecheck

pathToCGPath :: Path -> IO CGPath
pathToCGPath path
  = do
    { mutableCGPath@(CGMutablePath fptr) <- $(objc [] $ Class ''CGMutablePath <: [cexp| CGPathCreateMutable() |])
    ; mapM_ (addPathElement mutableCGPath) path
    ; return $ CGPath (castForeignPtr fptr)      -- unsafe freeze
    }
  where
    addPathElement path (MoveToPoint (Point {..}))
     -- FIXME: language-c-inline needs to look through type synonyms
     -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
      = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''Double, 'pointY :> ''Double] $ void
          [cexp| CGPathMoveToPoint(path, NULL, pointX, pointY) |])
    addPathElement path (AddLineToPoint (Point {..}))
     -- FIXME: language-c-inline needs to look through type synonyms
     -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
      = if cgMutablePathIsEmpty path
        then
          error "Graphics.SpriteKit: 'AddLineToPoint' requires a non-empty graphics path"
        else 
          $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''Double, 'pointY :> ''Double] $ void
            [cexp| CGPathAddLineToPoint(path, NULL, pointX, pointY) |])
    addPathElement path (AddQuadCurveToPoint (Point {pointX = cpx, pointY = cpy}) (Point {pointX = x, pointY = y}))
     -- FIXME: language-c-inline needs to look through type synonyms
     -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
      = if cgMutablePathIsEmpty path
        then
          error "Graphics.SpriteKit: 'AddQuadCurveToPoint' requires a non-empty graphics path"
        else 
          $(objc ['path :> Class ''CGMutablePath, 'cpx :> ''Double, 'cpy :> ''Double, 'x :> ''Double, 'y :> ''Double] $ void
            [cexp| CGPathAddQuadCurveToPoint(path, NULL, cpx, cpy, x, y) |])
    addPathElement path (AddCurveToPoint (Point {pointX = cp1x, pointY = cp1y}) 
                                         (Point {pointX = cp2x, pointY = cp2y})
                                         (Point {pointX = x,    pointY = y}))
     -- FIXME: language-c-inline needs to look through type synonyms
     -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
      = if cgMutablePathIsEmpty path
        then
          error "Graphics.SpriteKit: 'AddCurveToPoint' requires a non-empty graphics path"
        else 
          $(objc ['path :> Class ''CGMutablePath, 'cp1x :> ''Double, 'cp1y :> ''Double, 
                                                  'cp2x :> ''Double, 'cp2y :> ''Double, 
                                                  'x    :> ''Double, 'y    :> ''Double] $ void
            [cexp| CGPathAddCurveToPoint(path, NULL, cp1x, cp1y, cp2x, cp2y, x, y) |])
    addPathElement path CloseSubpath
    -- FIXME: language-c-inline needs to look through type synonyms
    -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
     = $(objc ['path :> Class ''CGMutablePath] $ void
         [cexp| CGPathCloseSubpath(path) |])

cgPathToPath :: CGPath -> IO Path
cgPathToPath = error "SpriteKit: 'CGPath's cannot be marhsalled to Haskell yet"

cgMutablePathIsEmpty :: CGMutablePath -> Bool
cgMutablePathIsEmpty mpath = unsafePerformIO $
  $(objc ['mpath :> Class ''CGMutablePath] $ ''Bool <: [cexp| CGPathIsEmpty(mpath) |])

objc_emit

path_initialise = objc_initialise
