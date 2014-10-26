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
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Texture


-- FIXME: or should we factorise into a two-level structure? (but that would make it awkward to use record updates)
data Node = Sprite 
            { nodeName     :: String        -- ^The node identifier (doesn't have to be unique) — FIXME: should it be Maybe String
            , nodePosition :: Point         -- ^The position of the node in its parent's coordinate system.
            -- children???
            , nodeSize     :: Size          -- ^The dimensions of the sprite, in points.
            , nodeColor    :: Color         -- ^The sprite’s color.
            , nodeTexture  :: Maybe Texture
            }
          | Nodes
            { nodeName     :: String        -- ^The node identifier (doesn't have to be unique) — FIXME: should it be Maybe String
            , nodePosition :: Point         -- ^The position of the node in its parent's coordinate system.
            , nodeChildren :: [Node]
            }


spriteWithColor :: Color -> Size -> Node
spriteWithColor color size = Sprite 
                             { nodeName     = ""
                             , nodePosition = pointZero
                             , nodeSize     = size
                             , nodeColor    = color
                             , nodeTexture  = Nothing 
                             }

spriteWithImageNamed :: FilePath -> Node
spriteWithImageNamed imageName 
  = Sprite 
    { nodeName     = ""
    , nodePosition = pointZero
    , nodeSize     = textureSize texture
    , nodeColor    = whiteColor
    , nodeTexture  = Just texture 
    }
  where
    texture = textureWithImageNamed imageName

-- other properties are set using record updates

nodes :: [Node] -> Node
nodes children = Nodes { nodeName = "", nodePosition = pointZero, nodeChildren = children }
