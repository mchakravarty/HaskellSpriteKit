{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}

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

  -- * SpriteKit node representation
  Node(..),

  -- * Generic SpriteKit node functionality  
  node,
  
  -- * Sprite nodes
  -- spriteWithColorSize, spriteWithImageFile, spriteWithImageNamed, spriteWithTexture, spriteWithTextureColorSize, 
  spriteWithColorSize, spriteWithImageNamed, spriteWithTexture, spriteWithTextureColorSize, 
  
  -- * Marshalling support
  SKNode(..),
  nodeToSKNode, nodeToForeignPtr,
  
  node_initialise
) where

  -- standard libraries
import Control.Applicative
import Data.Maybe
import Data.Typeable
import Foreign          hiding (void)
import System.IO.Unsafe (unsafePerformIO)

  -- friends
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Texture

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- |Tree structure of SpriteKit nodes that are used to assemble scenes.
--
-- FIXME: or should we factorise into a two-level structure? (but that would make it awkward to use record updates)
data Node 
  = Node
    { nodeName               :: Maybe String  -- ^Optional node identifier (doesn't have to be unique)
    , nodePosition           :: Point         -- ^The position of the node in its parent's coordinate system.
    , nodeChildren           :: [Node]
    }
  | Sprite 
    { nodeName               :: Maybe String  -- ^Optional node identifier (doesn't have to be unique)
    , nodePosition           :: Point         -- ^The position of the node in its parent's coordinate system.
    , nodeChildren           :: [Node]
    , spriteSize             :: Size          -- ^The dimensions of the sprite, in points.
    , spriteAnchorPoint      :: Point         -- ^The point in the sprite that corresponds to the node’s position.
                                              -- ^In unit coordinate space; default: (0.5,0.5); i.e., centered on its position.
    , spriteTexture          :: Maybe Texture
    -- , spriteCenterRect      :: Rect  -- FIXME: not yet supported
    , spriteColorBlendFactor :: Float         -- ^Default = 0 ('spriteColor' is ignored when drawing texture)
                                              -- ^value >0 means texture is blended with 'spriteColour' before being drawn
    , spriteColor            :: Color         -- ^The sprite’s color.
    } 


-- General nodes
-- -------------

-- |Create a node that combines multiple child nodes, but doesn't have a visual representation of its own.
--
node :: [Node] -> Node
node children = Node { nodeName = Nothing, nodePosition = pointZero, nodeChildren = children }

-- Sprite nodes
-- ------------

-- |Create a coloured sprite of a given size.
--
spriteWithColorSize :: Color -> Size -> Node
spriteWithColorSize color size 
  = Sprite 
    { nodeName               = Nothing
    , nodePosition           = pointZero
    , nodeChildren           = []
    , spriteSize             = size
    , spriteAnchorPoint      = Point 0.5 0.5
    , spriteTexture          = Nothing 
    , spriteColorBlendFactor = 0
    , spriteColor            = color
    }

{-
-- |Create a texture sprite from an arbitrary image file.
--
-- A placeholder image is used if the file cannot be loaded.
--
spriteWithImageFile :: FilePath -> Node
spriteWithImageFile imageFile = spriteWithTexture (textureWithImageFile imageFile)
-}

-- |Create a texture sprite from an image in the app bundle (either a file or an image in a texture atlas).
--
-- A placeholder image is used if the image cannot be loaded.
--
-- NB: This function is not useful for interactive development. Use 'spriteWithImageFile' instead.
--
spriteWithImageNamed :: FilePath -> Node
spriteWithImageNamed imageName = spriteWithTexture (textureWithImageNamed imageName)

-- |Create a textured sprite from an in-memory texture.
--
spriteWithTexture :: Texture -> Node
spriteWithTexture texture = spriteWithTextureColorSize texture whiteColor (textureSize texture)

-- |Create a textured sprite from an in-memory texture, but also set an explicit colour and size.
--
-- NB: To colourise the texture, you also need to set the 'colorBlendFactor' field of the sprite.
--
spriteWithTextureColorSize :: Texture -> Color -> Size -> Node
spriteWithTextureColorSize texture color size
  = Sprite 
    { nodeName               = Nothing
    , nodePosition           = pointZero
    , nodeChildren           = []
    , spriteSize             = size
    , spriteAnchorPoint      = Point 0.5 0.5
    , spriteTexture          = Just texture 
    , spriteColorBlendFactor = 0
    , spriteColor            = color
    }

-- FIXME: Features not yet supported:
--   * custom 'blendMode', custom 'centerRect' (for texture scaling)
--
-- FIXME: Yosemite-only features not yet supported:
--   * Create sprites with normal maps with 'spriteNodeWithImageNamed:normalMapped:' and 'spriteNodeWithTexture:normalMap:'.
--   * Lighting support (properties): 'lightingBitMask', 'shadowedBitMask', 'shadowCastBitMask', and 'normalTexture'
--   * Custom shader support (property): shader


-- Marshalling
-- -----------

objc_marshaller 'pointToCGPoint 'cgPointToPoint
objc_marshaller 'sizeToCGSize   'cgSizeToSize

newtype SKNode = SKNode (ForeignPtr SKNode)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

nodeToSKNode :: Node -> IO SKNode
nodeToSKNode (Node {..})
  = do
    { node <- $(objc ['nodeName :> [t| Maybe String |], 'nodePosition :> ''Point] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKNode *node = [SKNode node];
                  node.position         = *nodePosition;
                  node.name             = nodeName;
                  free(nodePosition);
                  node; 
                }) |])
    ; addChildren node nodeChildren
    ; return node
    }
nodeToSKNode (Sprite {..})
  = do
    { spriteTextureOrNil <- case spriteTexture of
                              Nothing            -> SKTexture <$> newForeignPtr_ nullPtr
                              Just spriteTexture -> return spriteTexture
    ; node <- $(objc [ 'nodeName               :> [t| Maybe String |]
                     , 'nodePosition           :> ''Point
                     , 'spriteSize             :> ''Size
                     , 'spriteAnchorPoint      :> ''Point
                     , 'spriteTextureOrNil     :> Class ''SKTexture
                     , 'spriteColorBlendFactor :> ''Float
                     , 'spriteColor            :> Class ''SKColor
                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKSpriteNode *node = [[SKSpriteNode alloc] initWithTexture:spriteTextureOrNil 
                                                                                color:spriteColor
                                                                                 size:*spriteSize];
                  node.name             = nodeName;
                  node.position         = *nodePosition;
                  node.anchorPoint      = *spriteAnchorPoint;
                  node.colorBlendFactor = spriteColorBlendFactor;
                  free(nodePosition);
                  free(spriteSize);
                  free(spriteAnchorPoint);
                  node; 
                }) |])
    ; addChildren node nodeChildren 
    ; return node
    }

addChildren :: SKNode -> [Node] -> IO ()
addChildren parent children
  = mapM_ (addElement parent) children
  where
    addElement parent child
      = do
        { skChild <- nodeToSKNode child
        ; $(objc ['parent :> Class ''SKNode, 'skChild :> Class ''SKNode] $ void [cexp| [parent addChild:skChild] |])
        }

nodeToForeignPtr :: Node -> IO (ForeignPtr SKNode)
nodeToForeignPtr node = do { SKNode fptr <- nodeToSKNode node; return fptr }

objc_emit

node_initialise = objc_initialise
