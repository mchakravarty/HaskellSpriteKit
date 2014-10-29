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
  Node(..),
  spriteWithColor, spriteWithImageNamed, node,

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


-- FIXME: or should we factorise into a two-level structure? (but that would make it awkward to use record updates)
data Node = Node
            { nodeName     :: String        -- ^The node identifier (doesn't have to be unique) — FIXME: should it be Maybe String
            , nodePosition :: Point         -- ^The position of the node in its parent's coordinate system.
            , nodeChildren :: [Node]
            }
          | Sprite 
            { nodeName     :: String        -- ^The node identifier (doesn't have to be unique) — FIXME: should it be Maybe String
            , nodePosition :: Point         -- ^The position of the node in its parent's coordinate system.
            , nodeChildren :: [Node]
            , nodeSize     :: Size          -- ^The dimensions of the sprite, in points.
            , nodeColor    :: Color         -- ^The sprite’s color.
            , nodeTexture  :: Maybe Texture
            }          
-- FIXME: name must be optional!

node :: [Node] -> Node
node children = Node { nodeName = "", nodePosition = pointZero, nodeChildren = children }

spriteWithColor :: Color -> Size -> Node
spriteWithColor color size = Sprite 
                             { nodeName     = ""
                             , nodePosition = pointZero
                             , nodeChildren = []
                             , nodeSize     = size
                             , nodeColor    = color
                             , nodeTexture  = Nothing 
                             }

spriteWithImageNamed :: FilePath -> Node
spriteWithImageNamed imageName 
  = Sprite 
    { nodeName     = ""
    , nodePosition = pointZero
    , nodeChildren = []
    , nodeSize     = textureSize texture
    , nodeColor    = whiteColor
    , nodeTexture  = Just texture 
    }
  where
    texture = textureWithImageNamed imageName

-- other properties are set using record updates


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
    { node <- $(objc ['nodeName :> ''String, 'nodePosition :> ''Point] $ Class ''SKNode <:
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
    { nodeTextureOrNil <- case nodeTexture of
                            Nothing          -> SKTexture <$> newForeignPtr_ nullPtr
                            Just nodeTexture -> return nodeTexture
    ; node <- $(objc [ 'nodeName         :> ''String 
                     , 'nodePosition     :> ''Point
                     , 'nodeSize         :> ''Size
                     , 'nodeColor        :> Class ''SKColor
                     , 'nodeTextureOrNil :> Class ''SKTexture
                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKNode *node = [[SKSpriteNode alloc] initWithTexture:nodeTextureOrNil color:nodeColor size:*nodeSize];
                  node.position         = *nodePosition;
                  node.name             = nodeName;
                  free(nodePosition);
                  free(nodeSize);
                  node; 
                }) |])
    ; let SKNode fptr = node
    ; putStrLn $ "nodeToSKNode: " ++ show fptr
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
-- nodeToForeignPtr node = do { SKNode fptr <- nodeToSKNode node; return fptr }
nodeToForeignPtr node = do { 
  putStrLn $ "before nodeToSKNode"; SKNode fptr <- nodeToSKNode node; putStrLn $ "nodeToForeignPtr: "++show fptr; return fptr }  -- FIXME: remove print

-- objc_interface [cunit|
--   void Node_initialise(void);
-- |]

objc_emit

node_initialise = objc_initialise

-- foreign export ccall "Node_initialise" objc_initialise :: IO ()
