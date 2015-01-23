{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Cocoa SpriteKit for Haskell
--
-- Concepts:
--
-- * SpriteKit node trees are represented as conventional algebraic datatypes in Haskell.
--
-- * For rendering, Haskell node trees are converted to native 'SKNode' trees. For animated scenes, where all animation is
--   driven by Haskell, the conversion of updated scenes works by updating the previous 'SKNode' tree.
--
-- * For interactive scenes and scenes using SceneKit's actions or physics, the Haskell scene code gets called with a Haskell
--   tree representing the current 'SKNode' tree, which, after Haskell side processing, is converted to an 'SKNode' tree again
--   by updating the version that was passed to the Haskell code.


module Graphics.SpriteKit (

  -- * Basic geometry and similar
  module Graphics.SpriteKit.Geometry,

  -- * Color representation
  module Graphics.SpriteKit.Color,

  -- * Bit images and textures
  module Graphics.SpriteKit.Image,
  module Graphics.SpriteKit.Texture,

  -- * Vector paths
  module Graphics.SpriteKit.Path,

  -- * Nodes and scenes
  module Graphics.SpriteKit.Node,
  module Graphics.SpriteKit.Scene,
  
  -- * Actions animating nodes
  module Graphics.SpriteKit.Action,

  -- * User events
  module Graphics.SpriteKit.Event,

  -- * Internal  
  spritekit_initialise
) where

-- FIXME: We should hide the constructors of 'Scene' and 'Node' and only export the user-facing field names.
import Graphics.SpriteKit.Action   hiding (SKPath(..), actionToSKAction)
import Graphics.SpriteKit.Color    hiding (Color(..), SKColor(..), colorToSKColor)
import Graphics.SpriteKit.Color           (Color)
import Graphics.SpriteKit.Event    hiding (keyEvent, mouseEvent)
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Image    hiding (NSUIImage(..), imageToNSUIImage)
import Graphics.SpriteKit.Path     hiding (CGPath(..), pathToCGPath)
import Graphics.SpriteKit.Scene    hiding (SKScene(..), sceneToSKScene)
import Graphics.SpriteKit.Texture  hiding (SKTexture(..), textureToSKTexture)
import Graphics.SpriteKit.Node     hiding (SKNode(..), nodeToSKNode, addChildren,  addActionDirectives)

spritekit_initialise :: IO ()
spritekit_initialise 
  = do
    { action_initialise
    ; color_initialise
    ; event_initialise
    ; geometry_initialise
    ; image_initialise
    ; path_initialise
    ; scene_initialise
    ; texture_initialise
    ; node_initialise
    }

foreign export ccall spritekit_initialise :: IO ()
