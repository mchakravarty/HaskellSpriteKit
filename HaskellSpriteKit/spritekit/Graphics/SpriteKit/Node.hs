{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Node
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- SpriteKit nodes.


module Graphics.SpriteKit.Node (

  -- * SpriteKit node representation
  Directive(..), Node(..), NodeUpdate,
  
  -- * Action directives
  runAction, runActionWithKey, removeActionForKey, removeAllActions,

  -- * Generic SpriteKit node functionality  
  node,
  
  -- * Label nodes
  labelNodeWithFontNamed, labelNodeWithText,
  
  -- * Shape nodes
  shapeNodeWithPath,
  
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
import Graphics.SpriteKit.Action
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Path
import Graphics.SpriteKit.Texture
import Graphics.SpriteKit.Types

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- Action directives
-- -----------------

-- |Initiate a new action.
--
runAction :: Action userData -> Directive userData
runAction action = RunAction action Nothing

-- |Initiate a new action and give it a name.
--
-- If an action with the same name is currently underway on a node that receives this action, the old action is removed first.
--
runActionWithKey :: Action userData -> String -> Directive userData
runActionWithKey action key = RunAction action (Just key)

-- |Instructs to remove any action with the give name.
--
removeActionForKey :: String -> Directive userData
removeActionForKey = RemoveActionForKey

-- |Instructs to remove all actions from any node that receives this directive.
--
removeAllActions :: Directive userData
removeAllActions = RemoveAllActions


-- General nodes
-- -------------

-- |Create a node that combines multiple child nodes, but doesn't have a visual representation of its own.
--
node :: [Node userData] -> Node userData
node children 
  = Node 
    { nodeName             = Nothing
    , nodePosition         = pointZero
    , nodeZPosition        = 0.0
    , nodeXScale           = 1.0
    , nodeYScale           = 1.0
    , nodeZRotation        = 0.0
    , nodeChildren         = children 
    , nodeActionDirectives = []
    , nodeSpeed            = 1.0
    , nodePaused           = False
    , nodeUserData         = error "Graphics.SpriteKit.Node: uninitialised user data (Node)"
    }

-- FIXME: Features not yet supported:
--   * Load a scene from a '.sks' file with 'nodeWithFileNamed:' (unfortunately, the docs suggest that the file needs to be
--     contained in the app's main bundle — test that)
--   * query functions on all node variants: 'frame' (only relevant for subclasses, but we should define it on all flavours
--     of nodes) and 'calculateAccumulatedFrame'
--   * visibility record fields: 'alpha' and 'hidden'
--   * user interaction record field: 'userInteractionEnabled'
--   * useful auxilliary function (to be recorded on the Haskell representation): 'inParentHierarchy:'
--   * 'parent' and 'scene' back edges (as 'Maybe Node'): how important is this? best would be read only (as in SpriteKit),
--     but then all child manipulation would also have to go through functions that maintain the relationship.
--   * query helpers: 'childNodeWithName:', 'enumerateChildNodesWithName:usingBlock:' (as some
--     sort of 'map' function maybe) — 'objectForKeyedSubscript:' seems to make little sense
--     NB: We don't want to just reimplement these function in Haskell, as they support Xpath-ish search expressions. Once, we
--         cache the Haskell representation of a node in its 'userData', we can use the original SpriteKit functions and still
--         return the Haskell representation of a node without much overhead.
--         However, we still need to develop other patterns of use than used in ObjC/Swift as we cannot update a node in the tree
--         *inplace*.
--
--   * actions queries: 'actionForKey', 'hasActions'
--   * 'physicsBody'
--
--   All these require to translate the node tree to SKNode to get accurate results:
--   * coordinate conversion: 'convertPoint:fromNode:' and 'convertPoint:toNode:'
--   * hit tests: 'containsPoint:', 'nodeAtPoint:', 'nodesAtPoint:'
--   * 'intersectsNode:'
--
--   We should cache translated/native trees, but not export the fields holding the translation. By storing a stable pointer to
--   the Haskell representation in the 'userData' of the native representation, we can cheaply test whether a particular
--   Haskell-side node value is identical to the native representation it refers to.
--
-- FIXME: Yosemite-only features not yet supported:
--   * 'constraints' and 'reachConstraints'


-- Label nodes
-- -----------

-- |Creates a label node without any text, but using the specified font.
--
labelNodeWithFontNamed :: String -> Node userData
labelNodeWithFontNamed font
  = Label
    { nodeName             = Nothing
    , nodePosition         = pointZero
    , nodeZPosition        = 0.0
    , nodeXScale           = 1.0
    , nodeYScale           = 1.0
    , nodeZRotation        = 0.0
    , nodeChildren         = []
    , nodeActionDirectives = []
    , nodeSpeed            = 1.0
    , nodePaused           = False
    , nodeUserData         = error "Graphics.SpriteKit.Node: uninitialised user data (Label)"
    , labelText            = ""
    , labelFontColor       = whiteColor
    , labelFontName        = Just font
    , labelFontSize        = 32
    }

-- |Creates a label node with the given text (set in 32pt Helvetica Neue Ultralight).
--
labelNodeWithText :: String -> Node userData
labelNodeWithText text
  = Label
    { nodeName             = Nothing
    , nodePosition         = pointZero
    , nodeZPosition        = 0.0
    , nodeXScale           = 1.0
    , nodeYScale           = 1.0
    , nodeZRotation        = 0.0
    , nodeChildren         = []
    , nodeActionDirectives = []
    , nodeSpeed            = 1.0
    , nodePaused           = False
    , nodeUserData         = error "Graphics.SpriteKit.Node: uninitialised user data (Label)"
    , labelText            = text
    , labelFontColor       = whiteColor
    , labelFontName        = Just "Helvetica Neue Ultralight"
    , labelFontSize        = 32
    }

-- FIXME: Features not yet supported:
--   * properties: 'verticalAlignmentMode' & 'horizontalAlignmentMode' and 'color' and 'colorBlendFactor' and also 'blendMode'

-- Shape nodes
-- -----------

-- |Creates a shape node from a graphics path relative to the nodes origin.
--
shapeNodeWithPath :: Path -> Node userData
shapeNodeWithPath path
  = Shape
    { nodeName             = Nothing
    , nodePosition         = pointZero
    , nodeZPosition        = 0.0
    , nodeXScale           = 1.0
    , nodeYScale           = 1.0
    , nodeZRotation        = 0.0
    , nodeChildren         = []
    , nodeActionDirectives = []
    , nodeSpeed            = 1.0
    , nodePaused           = False
    , nodeUserData         = error "Graphics.SpriteKit.Node: uninitialised user data (Shape)"
    , shapePath            = path
    , shapeFillColor       = clearColor
    , shapeLineWidth       = 1.0
    , shapeGlowWidth       = 0.0
    , shapeAntialiased     = True
    , shapeStrokeColor     = whiteColor
    }

-- FIXME: Yosemite-only features not yet supported:
--   * 'shapeNodeWithPath:centered:' (what do we do about this, add a 'shapePathCentered' record field that is set
--     appropriately by the various constructors?)
--   * 'shapeNodeWithRect:', 'shapeNodeWithRectOfSize:', 'shapeNodeWithRect:cornerRadius:', 
--     'shapeNodeWithRectOfSize:cornerRadius:', 'shapeNodeWithCircleOfRadius:', 'shapeNodeWithEllipseOfSize:',
--     'shapeNodeWithEllipseInRect:', 'shapeNodeWithPoints:count:', 'shapeNodeWithSplinePoints:count:'
--     (all these, we can implement vor 10.9, by explicitly going through Core Graphics and translating the CGPath back
--     into Haskell; however, on 10.10, we can optimise, by already creating the SKShapeNode and only translating the
--     back back into Haskell lazily — i.e., if the record field is really being accessed, which won't happen much).
--   * 'fillTexture', 'fillShader', 'strokeTexture', and 'strokeShader' properties
--   * 'lineCap', 'lineJoin', and 'miterLimit' properties
--   * custom 'blendMode' and reading 'lineLength'


-- Sprite nodes
-- ------------

-- |Create a coloured sprite of a given size.
--
spriteWithColorSize :: Color -> Size -> Node userData
spriteWithColorSize color size 
  = Sprite 
    { nodeName               = Nothing
    , nodePosition           = pointZero
    , nodeZPosition          = 0.0
    , nodeXScale             = 1.0
    , nodeYScale             = 1.0
    , nodeZRotation          = 0.0
    , nodeChildren           = []
    , nodeActionDirectives   = []
    , nodeSpeed              = 1.0
    , nodePaused             = False
    , nodeUserData           = error "Graphics.SpriteKit.Node: uninitialised user data (Sprite)"
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
spriteWithImageNamed :: FilePath -> Node userData
spriteWithImageNamed imageName = spriteWithTexture (textureWithImageNamed imageName)

-- |Create a textured sprite from an in-memory texture.
--
spriteWithTexture :: Texture -> Node userData
spriteWithTexture texture = spriteWithTextureColorSize texture whiteColor (textureSize texture)

-- |Create a textured sprite from an in-memory texture, but also set an explicit colour and size.
--
-- NB: To colourise the texture, you also need to set the 'colorBlendFactor' field of the sprite.
--
spriteWithTextureColorSize :: Texture -> Color -> Size -> Node userData
spriteWithTextureColorSize texture color size
  = Sprite 
    { nodeName               = Nothing
    , nodePosition           = pointZero
    , nodeZPosition          = 0.0
    , nodeXScale             = 1.0
    , nodeYScale             = 1.0
    , nodeZRotation          = 0.0
    , nodeChildren           = []
    , nodeActionDirectives   = []
    , nodeSpeed              = 1.0
    , nodePaused             = False
    , nodeUserData           = error "Graphics.SpriteKit.Node: uninitialised user data (Sprite)"
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

-- FIXME: we need to include this somehow!!!
objc_interface [cunit|

typedef struct CGPath CGPath;
typedef struct CGPath CGMutablePath;

|]


objc_marshaller 'pointToCGPoint 'cgPointToPoint
objc_marshaller 'sizeToCGSize   'cgSizeToSize

newtype SKNode = SKNode (ForeignPtr SKNode)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

nodeToSKNode :: Node userData -> IO SKNode
nodeToSKNode (Node {..})
  = do
    { node <- $(objc [ 'nodeName      :> [t| Maybe String |]
                     , 'nodePosition  :> ''Point
  -- FIXME: language-c-inline needs to look through type synonyms
                     , 'nodeZPosition :> ''Double  -- should be ''GFloat
                     , 'nodeXScale    :> ''Double  -- should be ''GFloat
                     , 'nodeYScale    :> ''Double  -- should be ''GFloat
                     , 'nodeZRotation :> ''Double  -- should be ''GFloat
                     , 'nodeSpeed     :> ''Double  -- should be ''GFloat
                     , 'nodePaused    :> ''Bool
                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKNode *node = [SKNode node];
                  node.position         = *nodePosition;
                  node.zPosition        = nodeZPosition;
                  node.xScale           = nodeXScale;
                  node.yScale           = nodeYScale;
                  node.zRotation        = nodeZRotation;
                  node.name             = nodeName;
                  node.speed            = nodeSpeed;
                  node.paused           = nodePaused;
                  free(nodePosition);
                  node; 
                }) |])
    ; addChildren         node nodeChildren
    ; addActionDirectives node nodeActionDirectives
    ; return node
    }
nodeToSKNode (Label {..})
  = do
    { node <- $(objc [ 'nodeName       :> [t| Maybe String |]
                     , 'nodePosition   :> ''Point
  -- FIXME: language-c-inline needs to look through type synonyms
                     , 'nodeZPosition  :> ''Double  -- should be ''GFloat
                     , 'nodeXScale     :> ''Double  -- should be ''GFloat
                     , 'nodeYScale     :> ''Double  -- should be ''GFloat
                     , 'nodeZRotation  :> ''Double  -- should be ''GFloat
                     , 'nodeSpeed      :> ''Double  -- should be ''GFloat
                     , 'nodePaused     :> ''Bool
                     , 'labelText      :> ''String
                     , 'labelFontColor :> Class ''SKColor
                     , 'labelFontName  :> [t|Maybe String|]
                     , 'labelFontSize  :> ''Double  -- should be ''GFloat
                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKLabelNode *node = [SKLabelNode labelNodeWithText:labelText];
                  node.position         = *nodePosition;
                  node.zPosition        = nodeZPosition;
                  node.xScale           = nodeXScale;
                  node.yScale           = nodeYScale;
                  node.zRotation        = nodeZRotation;
                  node.name             = nodeName;
                  node.speed            = nodeSpeed;
                  node.paused           = nodePaused;
                  node.fontColor        = labelFontColor;
                  node.fontName         = labelFontName;
                  node.fontSize         = labelFontSize;
                  free(nodePosition);
                  node; 
                }) |])
    ; addChildren         node nodeChildren
    ; addActionDirectives node nodeActionDirectives
    ; return node
    }
nodeToSKNode (Shape {..})
  = do
    { cgPath <- pathToCGPath shapePath
    ; node <- $(objc [ 'nodeName               :> [t| Maybe String |]
                     , 'nodePosition           :> ''Point
                        -- FIXME: language-c-inline needs to look through type synonyms
                     , 'nodeZPosition          :> ''Double  -- should be ''GFloat
                     , 'nodeXScale             :> ''Double  -- should be ''GFloat
                     , 'nodeYScale             :> ''Double  -- should be ''GFloat
                     , 'nodeZRotation          :> ''Double  -- should be ''GFloat
                     , 'nodeSpeed              :> ''Double  -- should be ''GFloat
                     , 'nodePaused             :> ''Bool
                     , 'cgPath                 :> Class ''CGPath
                     , 'shapeFillColor         :> Class ''SKColor
  -- FIXME: language-c-inline needs to look through type synonyms
                     -- , 'nodeLineWidth          :> ''GFloat
                     , 'shapeLineWidth         :> ''Double
                     -- , 'nodeGlowWidth          :> ''GFloat
                     , 'shapeGlowWidth         :> ''Double
                     , 'shapeAntialiased       :> ''Bool
                     , 'shapeStrokeColor       :> Class ''SKColor
                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKShapeNode *node = [SKShapeNode shapeNodeWithPath:cgPath];
                  node.position              = *nodePosition;
                  node.zPosition             = nodeZPosition;
                  node.xScale                = nodeXScale;
                  node.yScale                = nodeYScale;
                  node.zRotation             = nodeZRotation;
                  node.name                  = nodeName;
                  node.speed                 = nodeSpeed;
                  node.paused                = nodePaused;
                  node.fillColor             = shapeFillColor;
                  node.lineWidth             = shapeLineWidth;
                  node.glowWidth             = shapeGlowWidth;
                  node.antialiased           = shapeAntialiased;
                  node.strokeColor           = shapeStrokeColor;
                  free(nodePosition);
                  node; 
                }) |])
    ; addChildren         node nodeChildren 
    ; addActionDirectives node nodeActionDirectives
    ; return node
    }
nodeToSKNode (Sprite {..})
  = do
    { spriteTextureOrNil <- case spriteTexture of
                              Nothing            -> SKTexture <$> newForeignPtr_ nullPtr
                              Just spriteTexture -> return spriteTexture
    ; node <- $(objc [ 'nodeName               :> [t| Maybe String |]
                     , 'nodePosition           :> ''Point
  -- FIXME: language-c-inline needs to look through type synonyms
                     , 'nodeZPosition          :> ''Double  -- should be ''GFloat
                     , 'nodeXScale             :> ''Double  -- should be ''GFloat
                     , 'nodeYScale             :> ''Double  -- should be ''GFloat
                     , 'nodeZRotation          :> ''Double  -- should be ''GFloat
                     , 'nodeSpeed              :> ''Double  -- should be ''GFloat
                     , 'nodePaused             :> ''Bool
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
                  node.position         = *nodePosition;
                  node.zPosition        = nodeZPosition;
                  node.xScale           = nodeXScale;
                  node.yScale           = nodeYScale;
                  node.zRotation        = nodeZRotation;
                  node.name             = nodeName;
                  node.speed            = nodeSpeed;
                  node.paused           = nodePaused;
                  node.anchorPoint      = *spriteAnchorPoint;
                  node.colorBlendFactor = spriteColorBlendFactor;
                  free(nodePosition);
                  free(spriteSize);
                  free(spriteAnchorPoint);
                  node; 
                }) |])
    ; addChildren         node nodeChildren 
    ; addActionDirectives node nodeActionDirectives
    ; return node
    }

addChildren :: SKNode -> [Node userData] -> IO ()
addChildren parent children
  = mapM_ addElement children
  where
    addElement child
      = do
        { skChild <- nodeToSKNode child
        ; $(objc ['parent :> Class ''SKNode, 'skChild :> Class ''SKNode] $ void [cexp| [parent addChild:skChild] |])
        }

addActionDirectives :: SKNode -> [Directive userData] -> IO ()
addActionDirectives node directives
  = mapM_ addDirective directives
  where
    addDirective (RunAction action maybe_key)
      = do
        { skAction <- actionToSKAction action
        ; $(objc ['node :> Class ''SKNode, 'skAction :> Class ''SKAction, 'maybe_key :> [t| Maybe String |]] $ void
            [cexp| (maybe_key) ? [node runAction:skAction withKey:maybe_key] : [node runAction:skAction] |])
        }
    addDirective (RemoveActionForKey key)
      = $(objc ['node :> Class ''SKNode, 'key :> ''String] $ void [cexp| [node removeActionForKey:key] |])
    addDirective RemoveAllActions
      = $(objc ['node :> Class ''SKNode] $ void [cexp| [node removeAllActions] |])

nodeToForeignPtr :: Node userData -> IO (ForeignPtr SKNode)
nodeToForeignPtr node = do { SKNode fptr <- nodeToSKNode node; return fptr }

objc_emit

node_initialise = objc_initialise
