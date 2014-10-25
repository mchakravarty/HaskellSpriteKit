-- |
-- Module      : Graphics.SpriteKit.Texture
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Textures


module Graphics.SpriteKit.Node (
  Node(..),
  spriteWithColor, spriteWithImageNamed, nodes
) where

  -- friends
import Graphics.SpriteKit.Texture

-- FIXME: or should we factorise into a two-level structure? (but that would make it awkward to use record updates)
data Node = Sprite 
            { name     :: String          -- ^The node identifier (doesn't have to be unique) — FIXME: should it be Maybe String
            , position :: Point           -- ^The position of the node in its parent's coordinate system.
            -- children???
            , size     :: Size            -- ^The dimensions of the sprite, in points.
            , color    :: Color           -- ^The sprite’s color.
            , texture  :: Maybe Texture
            }
          | Nodes
            { name     :: String          -- ^The node identifier (doesn't have to be unique) — FIXME: should it be Maybe String
            , position :: Point           -- ^The position of the node in its parent's coordinate system.
            , children :: [Node]
            }


spriteWithColor :: Color -> Size -> Node
spriteWithColor color size = Sprite { name = "", position = origin, size = size, color = color, texture = Nothing }

spriteWithImageNamed :: FilePath -> Node
spriteWithImageNamed imageName 
  = Sprite 
    { name     = ""
    , position = origin
    , size     = textureSize texture
    , color    = white
    , texture  = Just texture 
    }
  where
    texture = textureWithImageNamed imageName

-- other properties are set using record updates

nodes :: [Node] -> Node
nodes children = Nodes { name = "", position = origin, children = children }
