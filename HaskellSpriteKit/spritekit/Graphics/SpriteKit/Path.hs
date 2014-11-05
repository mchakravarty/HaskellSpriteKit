{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Path
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Geometry types and operations

module Graphics.SpriteKit.Path (
  Path, PathElement(..),

  -- * Marshalling functions (internal)
  CGPath(..),
  
  path_initialise
) where

  -- standard libraries
import Data.Typeable
import Foreign

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- |Description of a graphics path.
--
type Path = [PathElement]

-- |Elementary components of a graphics path.
--
data PathElement = MoveToPoint         Point               -- ^Starts a new subpath at the given point.
                 | AddLineToPoint      Point               -- ^Adds a line from the current to the given point.
                 | AddQuadCurveToPoint Point Point         -- ^Adds a quadratic curve with control and destination point.
                 | AddCurveToPoint     Point Point Point   -- ^Adds a cubic curve with two control and one destination point.
                 | CloseSubpath                            -- ^Closes and completes the current subpath.


newtype CGPath = CGPath (ForeignPtr CGPath)
  deriving Typeable   -- needed for now until migrating to new TH
  -- FIXME: Use free() as a finaliser or '-release', or a 'CGRelease'?
  --        How should language-c-inline distinguish? Check whether it is an object?

objc_emit

path_initialise = objc_initialise
