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
--
-- * For the moment, we build a node tree without an 'SKScene' node on the Haskell side. An 'SKScene' node with the appropriate
--   size is automatically added by the presenter. In the future, we may add the scene node conditionally; i.e., only if the 
--   root of the tree is not already a scene node.


module Graphics.SpriteKit (
  module Graphics.SpriteKit.Action,
  module Graphics.SpriteKit.Color,
  module Graphics.SpriteKit.Event,
  module Graphics.SpriteKit.Geometry,
  module Graphics.SpriteKit.Image,
  module Graphics.SpriteKit.Path,
  module Graphics.SpriteKit.Scene,
  module Graphics.SpriteKit.Texture,
  module Graphics.SpriteKit.Node,
  
  spritekit_initialise
) where

-- FIXME: We should hide the constructors of 'Scene' and 'Node' and only export the user-facing field names.
import Graphics.SpriteKit.Action   hiding (SKPath(..), actionToSKAction)
import Graphics.SpriteKit.Color    hiding (SKColor(..))
import Graphics.SpriteKit.Event    hiding (keyEvent)
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Image
import Graphics.SpriteKit.Path     hiding (CGPath(..), pathToCGPath)
import Graphics.SpriteKit.Scene    hiding (SKScene(..), sceneToSKScene)
import Graphics.SpriteKit.Texture  hiding (SKTexture(..))
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
