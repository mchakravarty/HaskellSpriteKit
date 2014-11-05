-- {-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : Graphics.SpriteKit
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
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
  module Graphics.SpriteKit.Color,
  module Graphics.SpriteKit.Geometry,
  module Graphics.SpriteKit.Texture,
  module Graphics.SpriteKit.Node,
  
  spritekit_initialise
) where

import Graphics.SpriteKit.Color    hiding (SKColor(..))
import Graphics.SpriteKit.Geometry hiding (SKNode(..), nodeToSKNode)
import Graphics.SpriteKit.Path     hiding (CGPath(..))
import Graphics.SpriteKit.Texture  hiding (SKTexture(..))
import Graphics.SpriteKit.Node

spritekit_initialise :: IO ()
spritekit_initialise = color_initialise >> geometry_initialise >> path_initialise >> texture_initialise >> node_initialise
