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
import Graphics.SpriteKit.Path
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
  | Shape
    { nodeName               :: Maybe String  -- ^Optional node identifier (doesn't have to be unique)
    , nodePosition           :: Point         -- ^The position of the node in its parent's coordinate system.
    , nodeChildren           :: [Node]
    , nodePath               :: Path          -- ^Graphics path as a series of shapes or lines.
    , nodeFillColor          :: Color         -- ^The color used to fill the shape (default: clear == not filled).
    , nodeLineWidth          :: GFloat        -- ^The width used to stroke the path (default: 1.0; should be <= 2.0).
    , nodeGlowWidth          :: GFloat        -- ^Glow extending outward from the stroked line (default: 0.0 == no glow).
    , nodeAntialiased        :: Bool          -- ^Smooth stroked path during drawing? (default: True).
    , nodeStrokeColor        :: Color         -- ^Colour used to stroke the shape (default: white; clear == no stroke).
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
    , spriteColorBlendFactor :: GFloat        -- ^Default = 0 ('spriteColor' is ignored when drawing texture)
                                              -- ^value >0 means texture is blended with 'spriteColour' before being drawn
    , spriteColor            :: Color         -- ^The sprite’s color.
    } 


-- General nodes
-- -------------

-- |Create a node that combines multiple child nodes, but doesn't have a visual representation of its own.
--
node :: [Node] -> Node
node children = Node { nodeName = Nothing, nodePosition = pointZero, nodeChildren = children }

-- FIXME: Features not yet supported:
--   * Load a scene from a '.sks' file with 'nodeWithFileNamed:' (unfortunately, the docs suggest that the file needs to be
--     contained in the app's main bundle — test that)
--   * query functions on all node variants: 'frame' (only relevant for subclasses, but we should define it on all flavours
--     of nodes) and 'calculateAccumulatedFrame'
--   * record field of all node variants: 'zPosition'
--   * scaling and rotation record fields: 'xScale', 'yScale', 'zRotation'
--   * visibility record fields: 'alpha' and 'hidden'
--   * user interaction record field: 'userInteractionEnabled'
--   * useful auxilliary function (to be recorded on the Haskell representation): 'inParentHierarchy:'
--   * 'parent' and 'scene' back edges (as 'Maybe Node'): how important is this? best would be read only (as in SpriteKit),
--     but then all child manipulation would also have to go through functions that maintain the relationship.
--   * query helpers (to be reimplemented in Haskell): 'childNodeWithName:', 'enumerateChildNodesWithName:usingBlock:' (as some
--     sort of 'map' function maybe) — 'objectForKeyedSubscript:' seems to make little sense
--
--   * all actions methods
--   * 'physicsBody'
--
--   All these require to translate the node tree to SKNode to get accurate results:
--   * coordinate conversion: 'convertPoint:fromNode:' and 'convertPoint:toNode:'
--   * hit tests: 'containsPoint:', 'nodeAtPoint:', 'nodesAtPoint:'
--   * 'intersectsNode:'
--
--   * Do we want to provide a custom field stored in 'userData', but then we'd need to parameterise the node type.
--
--   We should cache translated/native trees, but not export the fields holding the translation. By storing a stable pointer to
--   the Haskell representation in the 'userData' of the native representation, we can cheaply test whether a particular
--   Haskell-side node value is identical to the native representation it refers to.
--
-- FIXME: Yosemite-only features not yet supported:
--   * 'constraints' and 'reachConstraints'


-- Shape nodes
-- -----------

-- |Creates a shape node from a graphics path relative to the nodes origin.
--
shapeNodeWithPath :: Path -> Node
shapeNodeWithPath path
  = Shape
    { nodeName               = Nothing
    , nodePosition           = pointZero
    , nodeChildren           = []
    , nodePath               = path
    , nodeFillColor          = clearColor
    , nodeLineWidth          = 1.0
    , nodeGlowWidth          = 0.0
    , nodeAntialiased        = True
    , nodeStrokeColor        = whiteColor
    }

-- FIXME: Yosemite-only features not yet supported:
--   * ??which of the shape creating class methods??
--   * 'fillTexture', 'fillShader', 'strokeTexture', and 'strokeShader' properties
--   * 'lineCap', 'lineJoin', and 'miterLimit' properties
--   * custom 'blendMode' and reading 'lineLength'


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
  -- FIXME: language-c-inline needs to look through type synonyms
                     -- , 'spriteColorBlendFactor :> ''GFloat
                     , 'spriteColorBlendFactor :> ''Double
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
