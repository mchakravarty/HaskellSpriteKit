{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Scene
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- SpriteKit scene nodes.

module Graphics.SpriteKit.Scene (

  -- * Scene representation
  Scene(..),
  
  -- * Scene creation
  sceneWithSize,

  -- * Marshalling functions (internal)
  sceneToSKNode, sceneToForeignPtr,

  scene_initialise
) where

  -- standard libraries
import Data.Typeable
import Foreign          hiding (void)
import System.IO.Unsafe (unsafePerformIO)


  -- friends
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Node
import Graphics.SpriteKit.Types

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- Scene nodes
-- -----------

-- |SpriteKit scene description.
--
data Scene sceneData nodeData
  = Scene
    { sceneName             :: Maybe String   -- ^Optional scene node identifier (doesn't have to be unique)
    , sceneChildren         :: [Node nodeData]
    , sceneActionDirectives :: [Directive (Scene sceneData nodeData)]
    , sceneSpeed            :: GFloat         -- ^Speed modifier for all actions in the entire subtree (default: 1.0)
    , sceneData             :: sceneData      -- ^Application specific information (default: uninitialised!)
    , scenePaused           :: Bool           -- ^If 'True' all actions in the entire subtree are skipped (default: 'False').
    , sceneAnchorPoint      :: Point          -- ^Point in the view’s frame that corresponds to the scene’s origin in unit
                                              -- coordinate space (default: (0, 0) == lower-left corner of the view's frame).
    , sceneSize             :: Size           -- ^Dimensions of the scene in points.
    , sceneScaleMode        :: SceneScaleMode -- ^How the scene is defined to the enclosing view (default: 'SceneScaleModeFill').
    , sceneBackgroundColor  :: Color          -- ^Background colour (default: RGBA 0.15, 0.15, 0.15, 1.0).
    }

-- |The modes that determine how the scene’s area is mapped to the view that presents it.
--
data SceneScaleMode = SceneScaleModeFill          -- ^Scale each axis independently to match the views.
                    | SceneScaleModeAspectFill    -- ^Preserve the aspect ratio and scale to just fill the entire view.
                    | SceneScaleModeAspectFit     -- ^Preserve the aspect ratio and scale to just not crop any content.
                    | SceneScaleModeResizeFill    -- ^No scaling, but automatically resize the scene to match the view.


-- Scene creation
-- --------------

-- |A new scene of the given size.
--
sceneWithSize :: Size -> Scene sceneData nodeData
sceneWithSize size
  = Scene
    { sceneName             = Nothing
    , sceneChildren         = []
    , sceneActionDirectives = []
    , sceneSpeed            = 1
    , scenePaused           = False
    , sceneAnchorPoint      = Point 0 0
    , sceneSize             = size
    , sceneScaleMode        = SceneScaleModeFill
    , sceneBackgroundColor  = colorWithRGBA 0.15 0.15 0.15 1.0
    , sceneData             = error "Graphics.SpriteKit.Scene: uninitialised user data (Scene)"
    }

-- FIXME: Features not yet supported:
--   * 'didChangeSize:' method
--   * 'convertPointFromView:' and 'convertPointToView:' (requires a marshalled scene; so to be efficient we need to cache the
--     native representation)
--   * 'willMoveFromView:', 'didMoveFromView:', and 'view' (requires a Haskell representation of 'SKView's)
--   * 'SKSceneDelegate' methods as callbacks
--   * 'physicsWorld'


-- Marshalling support
-- -------------------

objc_marshaller 'pointToCGPoint 'cgPointToPoint
objc_marshaller 'sizeToCGSize   'cgSizeToSize

sceneScaleModeToSKSceneScaleMode :: SceneScaleMode -> CLong  -- actually 'NSInteger'
sceneScaleModeToSKSceneScaleMode SceneScaleModeFill       = sceneScaleModeFill
sceneScaleModeToSKSceneScaleMode SceneScaleModeAspectFill = sceneScaleModeAspectFill
sceneScaleModeToSKSceneScaleMode SceneScaleModeAspectFit  = sceneScaleModeAspectFit
sceneScaleModeToSKSceneScaleMode SceneScaleModeResizeFill = sceneScaleModeResizeFill

-- NB: Seperate bindings to cache the results
{-# NOINLINE sceneScaleModeFill #-}
sceneScaleModeFill       = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKSceneScaleModeFill |])
{-# NOINLINE sceneScaleModeAspectFill #-}
sceneScaleModeAspectFill = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKSceneScaleModeAspectFill |])
{-# NOINLINE sceneScaleModeAspectFit #-}
sceneScaleModeAspectFit  = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKSceneScaleModeAspectFit |])
{-# NOINLINE sceneScaleModeResizeFill #-}
sceneScaleModeResizeFill = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKSceneScaleModeResizeFill |])

sceneToSKNode :: Scene sceneData nodeData -> IO SKNode
sceneToSKNode (Scene {..})
  = do
    { let skSceneScaleMode = sceneScaleModeToSKSceneScaleMode sceneScaleMode
    ; node <- $(objc [ 'sceneName            :> [t| Maybe String |]
  -- FIXME: language-c-inline needs to look through type synonyms
                     , 'sceneSpeed           :> ''Double  -- should be ''GFloat
                     , 'scenePaused          :> ''Bool
                     , 'sceneAnchorPoint     :> ''Point
                     , 'sceneSize            :> ''Size
                     , 'skSceneScaleMode     :> ''CLong
                     , 'sceneBackgroundColor :> Class ''SKColor

                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKScene *node = [SKScene sceneWithSize:*sceneSize];
                  node.name              = sceneName;
                  node.speed             = sceneSpeed;
                  node.paused            = scenePaused;
                  node.anchorPoint       = *sceneAnchorPoint;
                  node.scaleMode         = skSceneScaleMode;
                  node.backgroundColor   = sceneBackgroundColor;
                  free(sceneAnchorPoint);
                  free(sceneSize);
                  (typename SKNode *)node; 
                }) |])
    ; addChildren         node sceneChildren
    ; addActionDirectives node sceneActionDirectives
    ; return node
    }

sceneToForeignPtr :: Scene sceneData nodeData -> IO (ForeignPtr SKNode)
sceneToForeignPtr node = do { SKNode fptr <- sceneToSKNode node; return fptr }

objc_emit

scene_initialise = objc_initialise
