{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls, MagicHash, ScopedTypeVariables, KindSignatures #-}

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

  -- ** SpriteKit node representation
  Directive(..), Node(..), TimedUpdate,
  
  -- ** Action directives
  runAction, runActionWithKey, removeActionForKey, removeAllActions,

  -- ** Generic SpriteKit node functionality  
  node,
  
  -- ** Label nodes
  labelNodeWithFontNamed, labelNodeWithText,
  
  -- ** Shape nodes
  shapeNodeWithPath,
  
  -- ** Sprite nodes
  spriteWithColorSize, spriteWithImageNamed, spriteWithTexture, spriteWithTextureSize, spriteWithTextureColorSize, 
  
  -- ** Internal marshalling support
  SKNode(..),
  nodeToSKNode, addChildren, addActionDirectives, unsafeInterleaveNSArrayTolistOfNode, updateChildren, nodeToForeignPtr,
  
  node_initialise
) where

  -- standard libraries
import Control.Applicative
import Data.Maybe
import Data.Typeable
import Control.Exception          as Exc
import Foreign                    hiding (void)
import Foreign.ForeignPtr.Unsafe
-- import GHC.Prim                   (Any, reallyUnsafePtrEquality#)
import GHC.Prim                   (reallyUnsafePtrEquality#)
import System.IO.Unsafe           (unsafePerformIO, unsafeInterleaveIO)
import Unsafe.Coerce              (unsafeCoerce)

  -- friends
import Graphics.SpriteKit.Action
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Path
import Graphics.SpriteKit.Texture
import Graphics.SpriteKit.Types -- hiding (Any(..))

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h", "HaskellSpriteKit/StablePtrBox.h", "Action_objc.h"]


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
    , nodeForeign          = Nothing
    }

-- FIXME: Features not yet supported:
--   * Load a scene from a '.sks' file with 'nodeWithFileNamed:' (unfortunately, the docs suggest that the file needs to be
--     contained in the app's main bundle — test that)
--   * query functions on all node variants: 'frame' (only relevant for subclasses, but we should define it on all flavours
--     of nodes) and 'calculateAccumulatedFrame' — to perform these calculations, we need to marshall nodes to their ObjC
--     representation; that should be performance ok once all the necessary caching is in place
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
    , nodeForeign          = Nothing
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
    , nodeForeign          = Nothing
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
    , nodeForeign          = Nothing
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
    , nodeForeign            = Nothing
    , spriteSize             = size
    , spriteAnchorPoint      = Point 0.5 0.5
    , spriteTexture          = Nothing 
    , spriteColorBlendFactor = 0
    , spriteColor            = color
    }

-- |Create a texture sprite from an image in the app bundle (either a file or an image in a texture atlas).
--
-- A placeholder image is used if the image cannot be loaded.
--
spriteWithImageNamed :: FilePath -> Node userData
spriteWithImageNamed imageName = spriteWithTexture (textureWithImageNamed imageName)

-- |Create a textured sprite from an in-memory texture.
--
spriteWithTexture :: Texture -> Node userData
spriteWithTexture texture = spriteWithTextureSize texture (textureSize texture)

-- |Create a textured sprite from an in-memory texture, but also set an explicit size (instead of using the texture's size).
--
spriteWithTextureSize :: Texture -> Size -> Node userData
spriteWithTextureSize texture size = spriteWithTextureColorSize texture whiteColor (textureSize texture)

-- |Create a textured sprite from an in-memory texture, but also set an explicit size (instead of using the texture's size).
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
    , nodeForeign            = Nothing
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

keepSKNode :: SKNode -> IO SKNode
keepSKNode = return

objc_marshaller 'keepSKNode 'keepSKNode

-- listOfNodeToNSArray :: [Node userData] -> IO (NSArray SKNode)
-- listOfNodeToNSArray nodes
--   = do
--     { marr <- $(objc [] $ Class [t|NSMutableArray SKNode|] <: [cexp| [NSMutableArray arrayWithCapacity:20] |])
--     ; mapM_ (addElement marr) nodes
--     ; return $ unsafeFreezeNSMutableArray marr
--     }
--   where
--     addElement marr node
--       = do
--         { skNode <- nodeToSKNode node
--         ; $(objc ['marr :> Class [t|NSMutableArray SKNode|], 'skNode :> ''SKNode] $ void [cexp| [marr addObject:skNode] |])
--         }
-- 
-- nsArrayTolistOfNode :: NSArray SKNode -> IO [Node userData]
-- nsArrayTolistOfNode arr
--   = do
--     { n <- $(objc ['arr :> Class [t|NSArray SKNode|]] $ ''Int <: [cexp| (int)arr.count |]) :: IO Int
--     ; Prelude.sequence [ $(objc ['arr :> Class [t|NSArray SKNode|], 'i :> ''Int] $ Class ''SKNode <: 
--                            [cexp| arr[(typename NSUInteger)i] |]) 
--                          >>= skNodeToNode
--                        | i <- [0..n-1]]
--     }
-- 
-- objc_marshaller 'listOfNodeToNSArray 'nsArrayTolistOfNode

listOfSKNodeToNSArray :: [SKNode] -> IO (NSArray SKNode)
listOfSKNodeToNSArray skNodes
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray SKNode|] <: [cexp| [NSMutableArray arrayWithCapacity:20] |])
    ; mapM_ (addElement marr) skNodes
    ; return $ unsafeFreezeNSMutableArray marr
    }
  where
    addElement marr skNode
      = $(objc ['marr :> Class [t|NSMutableArray SKNode|], 'skNode :> ''SKNode] $ void [cexp| [marr addObject:skNode] |])

nsArrayTolistOfSKNode :: NSArray SKNode -> IO [SKNode]
nsArrayTolistOfSKNode arr
  = do
    { n <- $(objc ['arr :> Class [t|NSArray SKNode|]] $ ''Int <: [cexp| (int)arr.count |]) :: IO Int
    ; Prelude.sequence [ $(objc ['arr :> Class [t|NSArray SKNode|], 'i :> ''Int] $ Class ''SKNode <: 
                           [cexp| arr[(typename NSUInteger)i] |]) 
                       | i <- [0..n-1]]
    }

objc_marshaller 'listOfSKNodeToNSArray 'nsArrayTolistOfSKNode

-- Extract all 'SKNode' references out of the 'NSArray', but wait with marshalling the nodes to their
-- Haskell representation until they are actually used.
--
unsafeInterleaveNSArrayTolistOfNode :: NSArray SKNode -> IO [Node userData]
unsafeInterleaveNSArrayTolistOfNode arr
  = do
    { n <- $(objc ['arr :> Class [t|NSArray SKNode|]] $ ''Int <: [cexp| (int)arr.count |]) :: IO Int
    ; skNodes <- Prelude.sequence [ $(objc ['arr :> Class [t|NSArray SKNode|], 'i :> ''Int] $ Class ''SKNode <: 
                                      [cexp| arr[(typename NSUInteger)i] |]) 
                                  | i <- [0..n-1]]
    ; mapM (unsafeInterleaveIO . skNodeToNode) skNodes
    }

nodeToSKNode :: Node userData -> IO SKNode
nodeToSKNode Node{..}
  = do
    { let nodeUserDataAny = unsafeCoerce (Box nodeUserData) :: Box Any   -- opaque data marshalled as a stable pointer
    ; node <- $(objc [ 'nodeName        :> [t| Maybe String |]
                     , 'nodePosition    :> ''Point
  -- FIXME: language-c-inline needs to look through type synonyms
                     , 'nodeZPosition   :> ''Double  -- should be ''GFloat
                     , 'nodeXScale      :> ''Double  -- should be ''GFloat
                     , 'nodeYScale      :> ''Double  -- should be ''GFloat
                     , 'nodeZRotation   :> ''Double  -- should be ''GFloat
                     , 'nodeSpeed       :> ''Double  -- should be ''GFloat
                     , 'nodePaused      :> ''Bool
                     , 'nodeUserDataAny :> [t| Box Any |]
                     , 'nodeForeign     :> [t| Maybe SKNode |]
                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKNode *node = (nodeForeign) ? nodeForeign : [SKNode node];
                  node.position         = *nodePosition;
                  node.zPosition        = nodeZPosition;
                  node.xScale           = nodeXScale;
                  node.yScale           = nodeYScale;
                  node.zRotation        = nodeZRotation;
                  node.name             = nodeName;
                  node.speed            = nodeSpeed;
                  node.paused           = nodePaused;
                  node.userData         = [NSMutableDictionary dictionaryWithObject:[StablePtrBox stablePtrBox:nodeUserDataAny]
                                                                             forKey:@"haskellUserData"];
                  free(nodePosition);
                  node; 
                }) |])
    ; addChildren (isNothing nodeForeign) node nodeChildren
    ; addActionDirectives node nodeActionDirectives
    ; return node
    }
nodeToSKNode Label{..}
  = do
    { let nodeUserDataAny  = unsafeCoerce (Box nodeUserData) :: Box Any   -- opaque data marshalled as a stable pointer
          skLabelFontColor = colorToSKColor labelFontColor
    ; node <- $(objc [ 'nodeName         :> [t| Maybe String |]
                     , 'nodePosition     :> ''Point
  -- FIXME: language-c-inline needs to look through type synonyms
                     , 'nodeZPosition    :> ''Double  -- should be ''GFloat
                     , 'nodeXScale       :> ''Double  -- should be ''GFloat
                     , 'nodeYScale       :> ''Double  -- should be ''GFloat
                     , 'nodeZRotation    :> ''Double  -- should be ''GFloat
                     , 'nodeSpeed        :> ''Double  -- should be ''GFloat
                     , 'nodePaused       :> ''Bool
                     , 'nodeUserDataAny  :> [t| Box Any |]
                     , 'nodeForeign      :> [t| Maybe SKNode |]
                     , 'labelText        :> ''String
                     , 'skLabelFontColor :> Class ''SKColor
                     , 'labelFontName    :> [t|Maybe String|]
                     , 'labelFontSize    :> ''Double  -- should be ''GFloat
                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKLabelNode *node = (typename SKLabelNode *)nodeForeign;
                  if (!node)
                    node = [SKLabelNode labelNodeWithFontNamed:labelFontName];
                  else
                    node.fontName = labelFontName;
                  node.position         = *nodePosition;
                  node.zPosition        = nodeZPosition;
                  node.xScale           = nodeXScale;
                  node.yScale           = nodeYScale;
                  node.zRotation        = nodeZRotation;
                  node.name             = nodeName;
                  node.speed            = nodeSpeed;
                  node.paused           = nodePaused;
                  node.userData         = [NSMutableDictionary dictionaryWithObject:[StablePtrBox stablePtrBox:nodeUserDataAny]
                                                                             forKey:@"haskellUserData"];
                  node.text             = labelText;
                  node.fontColor        = skLabelFontColor;
                  node.fontSize         = labelFontSize;
                  free(nodePosition);
                  node; 
                }) |])
    ; addChildren (isNothing nodeForeign) node nodeChildren
    ; addActionDirectives node nodeActionDirectives
    ; return node
    }
nodeToSKNode (Shape {..})
  = do
    { let nodeUserDataAny    = unsafeCoerce (Box nodeUserData) :: Box Any   -- opaque data marshalled as a stable pointer
          skShapeFillColor   = colorToSKColor shapeFillColor
          skShapeStrokeColor = colorToSKColor shapeStrokeColor
    ; cgPath <- pathToCGPath shapePath
    ; node <- $(objc [ 'nodeName               :> [t| Maybe String |]
                     , 'nodePosition           :> ''Point
                        -- FIXME: language-c-inline needs to look through type synonyms
                     , 'nodeZPosition          :> ''Double  -- should be ''GFloat
                     , 'nodeXScale             :> ''Double  -- should be ''GFloat
                     , 'nodeYScale             :> ''Double  -- should be ''GFloat
                     , 'nodeZRotation          :> ''Double  -- should be ''GFloat
                     , 'nodeSpeed              :> ''Double  -- should be ''GFloat
                     , 'nodePaused             :> ''Bool
                     , 'nodeUserDataAny        :> [t| Box Any |]
                     , 'nodeForeign            :> [t| Maybe SKNode |]
                     , 'cgPath                 :> Class ''CGPath
                     , 'skShapeFillColor       :> Class ''SKColor
  -- FIXME: language-c-inline needs to look through type synonyms
                     , 'shapeLineWidth         :> ''Double   -- should be ''GFloat
                     , 'shapeGlowWidth         :> ''Double   -- should be ''GFloat
                     , 'shapeAntialiased       :> ''Bool
                     , 'skShapeStrokeColor     :> Class ''SKColor
                     ] $ Class ''SKNode <:
                [cexp| ({                 
                  typename SKShapeNode *node = (typename SKShapeNode *)nodeForeign;
                  if (!node) {
                    if ([SKShapeNode resolveClassMethod:@selector(shapeNodeWithPath:)])
                      node = [SKShapeNode shapeNodeWithPath:cgPath];
                    else {
                      node      = [[SKShapeNode alloc] init];
                      node.path = cgPath;
                    }
                  } else
                    node.path = cgPath;
                  node.position              = *nodePosition;
                  node.zPosition             = nodeZPosition;
                  node.xScale                = nodeXScale;
                  node.yScale                = nodeYScale;
                  node.zRotation             = nodeZRotation;
                  node.name                  = nodeName;
                  node.speed                 = nodeSpeed;
                  node.paused                = nodePaused;
                  node.userData              = [NSMutableDictionary 
                                                dictionaryWithObject:[StablePtrBox stablePtrBox:nodeUserDataAny]
                                                              forKey:@"haskellUserData"];
                  //FIXME: Leads to a crash in OpenGL rendering — appears to be a SpriteKit bug.
                  //node.fillColor             = skShapeFillColor;
                  node.lineWidth             = shapeLineWidth;
                  node.glowWidth             = shapeGlowWidth;
                  node.antialiased           = shapeAntialiased;
                  node.strokeColor           = skShapeStrokeColor;
                  free(nodePosition);
                  node; 
                }) |])
    ; addChildren (isNothing nodeForeign) node nodeChildren
    ; addActionDirectives node nodeActionDirectives
    ; return node
    }
nodeToSKNode Sprite{..}
  = do
   { let nodeUserDataAny = unsafeCoerce (Box nodeUserData) :: Box Any   -- opaque data marshalled as a stable pointer
         skSpriteColor   = colorToSKColor spriteColor
    ; spriteTextureOrNil <- case spriteTexture of
                              Nothing            -> SKTexture <$> newForeignPtr_ nullPtr
                              Just spriteTexture -> return $ textureToSKTexture spriteTexture
    ; node <- $(objc [ 'nodeName               :> [t| Maybe String |]
                     , 'nodePosition           :> ''Point
  -- FIXME: language-c-inline needs to look through type synonyms
                     , 'nodeZPosition          :> ''Double  -- should be ''GFloat
                     , 'nodeXScale             :> ''Double  -- should be ''GFloat
                     , 'nodeYScale             :> ''Double  -- should be ''GFloat
                     , 'nodeZRotation          :> ''Double  -- should be ''GFloat
                     , 'nodeSpeed              :> ''Double  -- should be ''GFloat
                     , 'nodePaused             :> ''Bool
                     , 'nodeUserDataAny        :> [t| Box Any |]
                     , 'nodeForeign            :> [t| Maybe SKNode |]
                     , 'spriteSize             :> ''Size
                     , 'spriteAnchorPoint      :> ''Point
                     , 'spriteTextureOrNil     :> Class ''SKTexture
  -- FIXME: language-c-inline needs to look through type synonyms
                     -- , 'spriteColorBlendFactor :> ''GFloat
                     , 'spriteColorBlendFactor :> ''Double
                     , 'skSpriteColor          :> Class ''SKColor
                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename SKSpriteNode *node = (typename SKSpriteNode *)nodeForeign;
                  if (!node) 
                    node = [[SKSpriteNode alloc] initWithTexture:spriteTextureOrNil color:skSpriteColor size:*spriteSize];
                  else {
                    node.texture = spriteTextureOrNil;
                    node.color   = skSpriteColor;
                    node.size    = *spriteSize;
                  }
                  node.position         = *nodePosition;
                  node.zPosition        = nodeZPosition;
                  node.xScale           = nodeXScale;
                  node.yScale           = nodeYScale;
                  node.zRotation        = nodeZRotation;
                  node.name             = nodeName;
                  node.speed            = nodeSpeed;
                  node.paused           = nodePaused;
                  node.userData         = [NSMutableDictionary dictionaryWithObject:[StablePtrBox stablePtrBox:nodeUserDataAny]
                                                                             forKey:@"haskellUserData"];
                  node.anchorPoint      = *spriteAnchorPoint;
                  node.colorBlendFactor = spriteColorBlendFactor;
                  free(nodePosition);
                  free(spriteSize);
                  free(spriteAnchorPoint);
                  node; 
                }) |])
    ; addChildren (isNothing nodeForeign) node nodeChildren
    ; addActionDirectives node nodeActionDirectives
    ; return node
    }

-- Marshal the given list of child nodes and add them to the given parent 'SKNode'; the latter only if the child is
-- not yet in the list of children.
--
-- A newly marshalled (and hence, updated) child node can already be in the list of children if its 'nodeForeign'
-- value is not 'Nothing'.
--
-- We never remove childen from newly created 'SKNode's, as these are system-created children, such as the text
-- texture of an 'SKLabel'.
--
addChildren :: Bool -> SKNode -> [Node userData] -> IO ()
addChildren newNode parent newChildren
  = do
    { newSKChildren <- mapM nodeToSKNode newChildren

        -- (1) We remove all children from the parent that do not appear in `newChildren`.
        -- (2) We add all elements from `newChildren` to the parent that are not yet its children.
    ; $(objc ['newNode :> ''Bool, 'parent :> ''SKNode, 'newSKChildren :> [t| [SKNode] |]] $ void
        [cexp| ({ 
//        for (typename SKNode *child in parent.children) if (![newSKChildren   containsObject:child]) [child removeFromParent];
//        for (typename SKNode *child in newSKChildren)   if (![parent.children containsObject:child]) [parent addChild:child];
          typename SKNode *child;

          typename NSEnumerator *enumerator = [parent.children objectEnumerator];
          if (!newNode)
            while ((child = [enumerator nextObject]) != nil)
              if (![newSKChildren containsObject:child]) [child removeFromParent];

          enumerator = [newSKChildren objectEnumerator];
          while ((child = [enumerator nextObject]) != nil)
            if (![parent.children containsObject:child]) [parent addChild:child];
        }) |])     
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
      
skNodeToNode :: SKNode -> IO (Node userData)
skNodeToNode skNode
  = do
    { className <- $(objc ['skNode :> Class ''SKNode] $ ''String <: [cexp| [skNode className] |])
    
        -- We need to get the user data eagerly as we want to avoid evaluating user data unless it is used by the application
        -- (after all, we initialised it to 'error' by default). If we do *both* lazily, we create thunk chains spanning the
        -- Haskell and Objective-C representation of SpriteKit nodes.
    ; Box nodeUserData <- do
                          { userDataAny <- $(objc ['skNode :> Class ''SKNode] $  [t| Maybe (Box Any) |] <: 
                                             [cexp| ((typename StablePtrBox *)[skNode.userData 
                                                                               objectForKey:@"haskellUserData"]).stablePtr |])
                          ; case userDataAny of 
                              Nothing  -> error "accessed 'nodeUserData' of a foreign node"
                              Just box -> unsafeCoerce box
                          }
    ; case className of
        "SKLabelNode"  -> return $ Label  {..}
        "SKShapeNode"  -> return $ Shape  {..}
        "SKSpriteNode" -> return $ Sprite {..}        
          -- We treat everything else as an 'SKNode' (which is ok as long as only the common fields are used Haskell side)
        _              -> putStrLn ("skNodeToNode: catch all: " ++ className) >> (return $ Node   {..})
    }
    where
      nodeName             = unsafePerformIO 
                               $(objc ['skNode :> Class ''SKNode] $ [t| Maybe String |] <: [cexp| skNode.name |])
      nodePosition         = unsafePerformIO
                               $(objc ['skNode :> Class ''SKNode] $ ''Point <:
                                 [cexp| ({
                                   typename CGPoint *pnt = (typename CGPoint *) malloc(sizeof(CGPoint)); 
                                   *pnt = skNode.position;
                                   pnt;
                                  }) |])
      nodeZPosition        = unsafePerformIO
                               $(objc ['skNode :> Class ''SKNode] $  ''Double <: {-''GFloat-} [cexp| skNode.zPosition |])
      nodeXScale           = unsafePerformIO
                               $(objc ['skNode :> Class ''SKNode] $  ''Double <: {-''GFloat-} [cexp| skNode.xScale |])
      nodeYScale           = unsafePerformIO
                               $(objc ['skNode :> Class ''SKNode] $  ''Double <: {-''GFloat-} [cexp| skNode.yScale |])
      nodeZRotation        = unsafePerformIO
                               $(objc ['skNode :> Class ''SKNode] $  ''Double <: {-''GFloat-} [cexp| skNode.zRotation |])
      nodeChildren         = unsafePerformIO $ do
                             { arr <- $(objc ['skNode :> Class ''SKNode] $  Class [t| NSArray SKNode |] <: 
                                        [cexp| skNode.children |])
                             ; unsafeInterleaveNSArrayTolistOfNode arr
                             }
      nodeActionDirectives = []
      nodeSpeed            = unsafePerformIO
                               $(objc ['skNode :> Class ''SKNode] $  ''Double <: {-''GFloat-} [cexp| skNode.speed |])
      nodePaused           = unsafePerformIO
                               $(objc ['skNode :> Class ''SKNode] $  ''Bool <: [cexp| skNode.paused |])
      nodeForeign          = Just skNode
      --
      labelText            = unsafePerformIO 
                               $(objc ['skNode :> Class ''SKNode] $ ''String <: 
                                 [cexp| ((typename SKLabelNode *)skNode).text |])
      labelFontColor       = unsafePerformIO $
                               Color <$>
                                 $(objc ['skNode :> Class '' SKNode] $ Class ''SKColor <: 
                                   [cexp| ((typename SKLabelNode *)skNode).fontColor |])
      labelFontName        = unsafePerformIO 
                               $(objc ['skNode :> Class ''SKNode] $ [t| Maybe String |] <: 
                                 [cexp| ((typename SKLabelNode *)skNode).fontName |])
      labelFontSize        = unsafePerformIO 
                               $(objc ['skNode :> Class ''SKNode] $ ''Double{-GFloat-} <: 
                                 [cexp| ((typename SKLabelNode *)skNode).fontSize |])
      --
      shapePath            = unsafePerformIO $ do
                             { cgPath <- $(objc ['skNode :> Class '' SKNode] $ Class ''CGPath <: 
                                           [cexp| (typename CGPath*)((typename SKShapeNode *)skNode).path |])
                             ; cgPathToPath cgPath
                             }
      shapeFillColor       = unsafePerformIO $
                               Color <$>
                                 $(objc ['skNode :> Class '' SKNode] $ Class ''SKColor <: 
                                   [cexp| ((typename SKShapeNode *)skNode).fillColor |])
      shapeLineWidth       = unsafePerformIO 
                               $(objc ['skNode :> Class ''SKNode] $ ''Double{-GFloat-} <: 
                                 [cexp| ((typename SKShapeNode *)skNode).lineWidth |])
      shapeGlowWidth       = unsafePerformIO 
                               $(objc ['skNode :> Class ''SKNode] $ ''Double{-GFloat-} <: 
                                 [cexp| ((typename SKShapeNode *)skNode).glowWidth |])
      shapeAntialiased     = unsafePerformIO 
                               $(objc ['skNode :> Class ''SKNode] $ ''Bool <: 
                                 [cexp| ((typename SKShapeNode *)skNode).antialiased |])
      shapeStrokeColor     = unsafePerformIO $
                               Color <$>
                                 $(objc ['skNode :> Class '' SKNode] $ Class ''SKColor <: 
                                   [cexp| ((typename SKShapeNode *)skNode).strokeColor |])
      --
      spriteSize           = unsafePerformIO 
                               $(objc [ 'skNode :> ''SKNode ] $ ''Size <: 
                                 [cexp| ({
                                   typename CGSize *sz = (typename CGSize *) malloc(sizeof(CGSize)); 
                                   *sz = ((typename SKSpriteNode*)skNode).size;
                                   sz;
                                 }) |])
      spriteAnchorPoint    = unsafePerformIO 
                               $(objc [ 'skNode :> ''SKNode ] $ ''Point <: 
                                 [cexp| ({
                                   typename CGPoint *pnt = (typename CGPoint *) malloc(sizeof(CGPoint)); 
                                   *pnt = ((typename SKSpriteNode*)skNode).anchorPoint;
                                   pnt;
                                 }) |])
      spriteTexture        = unsafePerformIO $ do
                             { tex@(SKTexture fptr) <- $(objc ['skNode :> Class '' SKNode] $ Class ''SKTexture <: 
                                                         [cexp| ((typename SKSpriteNode *)skNode).texture |])
                             ; return $ if unsafeForeignPtrToPtr fptr == nullPtr then Nothing else Just (Texture tex)
                             }
    --   spriteCenterRect      :: Rect  -- FIXME: not yet supported
      spriteColorBlendFactor
                           = unsafePerformIO 
                               $(objc ['skNode :> Class ''SKNode] $ ''Double{-GFloat-} <: 
                                 [cexp| ((typename SKSpriteNode *)skNode).colorBlendFactor |])
      spriteColor          = unsafePerformIO $
                               Color <$>
                                 $(objc ['skNode :> Class '' SKNode] $ Class ''SKColor <: 
                                   [cexp| ((typename SKSpriteNode *)skNode).color |])


nodeToForeignPtr :: Node userData -> IO (ForeignPtr SKNode)
nodeToForeignPtr node = do { SKNode fptr <- nodeToSKNode node; return fptr }

runCustomAction :: TimedUpdateBox Any -> SKNode -> Double{-CGFloat-} -> IO ()
runCustomAction customActionAny skNode elapsedTime
  = do
    { let TimedUpdateBox customAction = unsafeCoerce customActionAny
    ; oldNode <- skNodeToNode skNode 
    ; let newNode = customAction oldNode elapsedTime
    ; _ <- mergeSKNode oldNode newNode
    ; return ()
    }
    `Exc.catch` \exc -> do
    {   -- FIXME: This error needs to go into the results table of the playground.
    ; putStrLn $ "Graphics.SpriteKit: customAction: " ++ show (exc :: Exc.SomeException)
    ; return ()
    }

-- If the second argument is a new node derived from the first argument (old node), update the 'SKNode' in 'nodeForeign'
-- with the changes of the new node with respect to the old node. Otherwise, marshal the new node, ignoring the old one.
-- The new node is derived of the old one if they are of the same node kind and have the same 'nodeForeign' reference.
--
mergeSKNode :: Node userData -> Node userData -> IO SKNode
mergeSKNode Node { nodeForeign          = Just skNode, ..} 
            Node { nodeName             = newNodeName
                 , nodePosition         = newNodePosition
                 , nodeZPosition        = newNodeZPosition
                 , nodeXScale           = newNodeXScale
                 , nodeYScale           = newNodeYScale
                 , nodeZRotation        = newNodeZRotation
                 , nodeChildren         = newNodeChildren
                 , nodeActionDirectives = newNodeActionDirectives
                 , nodeSpeed            = newNodeSpeed
                 , nodePaused           = newNodePaused
                 , nodeUserData         = newNodeUserData
                 , nodeForeign          = Just newSKNode
                 }
  | skNode == newSKNode
  = do
    { addActionDirectives skNode newNodeActionDirectives       -- Execute all new action directives

        -- For every field in the scene object, update it if it changed.
        -- NB: Superflous updates of unchanged values are benign. They only affect performance
        --     negatively, but do not alter the semantics. This is important as an intervening GC
        --     might move some of the pointers that we compare unsafely.
        --
    ; updateNodeName skNode nodeName newNodeName
    ; updateNodePosition skNode nodePosition newNodePosition
    ; updateZPosition skNode nodeZPosition newNodeZPosition
    ; updateXScale skNode nodeXScale newNodeXScale
    ; updateYScale skNode nodeYScale newNodeYScale
    ; updateZRotation skNode nodeZRotation newNodeZRotation
    ; updateChildren skNode nodeChildren newNodeChildren
    ; updateSpeed skNode nodeSpeed newNodeSpeed
    ; updatePaused skNode nodePaused newNodePaused
    ; updateUserData skNode nodeUserData newNodeUserData
    ; return skNode
    }
mergeSKNode Label { nodeForeign          = Just skNode, ..} 
            Label { nodeName             = newNodeName
                  , nodePosition         = newNodePosition
                  , nodeZPosition        = newNodeZPosition
                  , nodeXScale           = newNodeXScale
                  , nodeYScale           = newNodeYScale
                  , nodeZRotation        = newNodeZRotation
                  , nodeChildren         = newNodeChildren
                  , nodeActionDirectives = newNodeActionDirectives
                  , nodeSpeed            = newNodeSpeed
                  , nodePaused           = newNodePaused
                  , nodeUserData         = newNodeUserData
                  , nodeForeign          = Just newSKNode
                  , labelText            = newLabelText
                  , labelFontColor       = newLabelFontColor
                  , labelFontName        = newLabelFontName
                  , labelFontSize        = newLabelFontSize
                  }
  | skNode == newSKNode
  = do
    { addActionDirectives skNode newNodeActionDirectives       -- Execute all new action directives

        -- For every field in the scene object, update it if it changed.
        -- NB: Superflous updates of unchanged values are benign. They only affect performance
        --     negatively, but do not alter the semantics. This is important as an intervening GC
        --     might move some of the pointers that we compare unsafely.
        --
    ; updateNodeName skNode nodeName newNodeName
    ; updateNodePosition skNode nodePosition newNodePosition
    ; updateZPosition skNode nodeZPosition newNodeZPosition
    ; updateXScale skNode nodeXScale newNodeXScale
    ; updateYScale skNode nodeYScale newNodeYScale
    ; updateZRotation skNode nodeZRotation newNodeZRotation
    ; updateChildren skNode nodeChildren newNodeChildren
    ; updateSpeed skNode nodeSpeed newNodeSpeed
    ; updatePaused skNode nodePaused newNodePaused
    ; updateUserData skNode nodeUserData newNodeUserData
    ; case reallyUnsafePtrEquality# labelText newLabelText of
        1# -> return ()
        _  -> $(objc [ 'skNode :> ''SKNode, 'newLabelText :> ''String ] $ void 
                [cexp| ((typename SKLabelNode *)skNode).text = newLabelText |])
    ; case reallyUnsafePtrEquality# labelFontColor newLabelFontColor of
        1# -> return ()
        _  -> let newSKLabelFontColor = colorToSKColor newLabelFontColor
              in
              $(objc [ 'skNode :> ''SKNode, 'newSKLabelFontColor :> Class ''SKColor ] $ void
                [cexp| ((typename SKLabelNode *)skNode).fontColor = newSKLabelFontColor |])
    ; case reallyUnsafePtrEquality# labelFontName newLabelFontName of
        1# -> return ()
        _  -> $(objc [ 'skNode :> ''SKNode, 'newLabelFontName :> [t| Maybe String |] ] $ void 
                [cexp| ((typename SKLabelNode *)skNode).fontName = newLabelFontName |])
    ; case reallyUnsafePtrEquality# labelFontSize newLabelFontSize of
        1# -> return ()
        _  -> $(objc [ 'skNode :> ''SKNode, 'newLabelFontSize :> ''Double{-GFloat-} ] $ void 
                [cexp| ((typename SKLabelNode *)skNode).fontSize = newLabelFontSize |])
    ; return skNode
    }
mergeSKNode Shape { nodeForeign          = Just skNode, ..} 
            Shape { nodeName             = newNodeName
                  , nodePosition         = newNodePosition
                  , nodeZPosition        = newNodeZPosition
                  , nodeXScale           = newNodeXScale
                  , nodeYScale           = newNodeYScale
                  , nodeZRotation        = newNodeZRotation
                  , nodeChildren         = newNodeChildren
                  , nodeActionDirectives = newNodeActionDirectives
                  , nodeSpeed            = newNodeSpeed
                  , nodePaused           = newNodePaused
                  , nodeUserData         = newNodeUserData
                  , nodeForeign          = Just newSKNode
                  , shapePath            = newShapePath
                  , shapeFillColor       = newShapeFillColor
                  , shapeLineWidth       = newShapeLineWidth
                  , shapeGlowWidth       = newShapeGlowWidth
                  , shapeAntialiased     = newShapeAntialiased
                  , shapeStrokeColor     = newShapeStrokeColor
                  }
  | skNode == newSKNode
  = do
    { addActionDirectives skNode newNodeActionDirectives       -- Execute all new action directives

        -- For every field in the scene object, update it if it changed.
        -- NB: Superflous updates of unchanged values are benign. They only affect performance
        --     negatively, but do not alter the semantics. This is important as an intervening GC
        --     might move some of the pointers that we compare unsafely.
        --
    ; updateNodeName skNode nodeName newNodeName
    ; updateNodePosition skNode nodePosition newNodePosition
    ; updateZPosition skNode nodeZPosition newNodeZPosition
    ; updateXScale skNode nodeXScale newNodeXScale
    ; updateYScale skNode nodeYScale newNodeYScale
    ; updateZRotation skNode nodeZRotation newNodeZRotation
    ; updateChildren skNode nodeChildren newNodeChildren
    ; updateSpeed skNode nodeSpeed newNodeSpeed
    ; updatePaused skNode nodePaused newNodePaused
    ; updateUserData skNode nodeUserData newNodeUserData
    ; case reallyUnsafePtrEquality# shapePath newShapePath of
        1# -> return ()
        _  -> do
              { newCGShapePath <- pathToCGPath newShapePath
              ; $(objc [ 'skNode :> ''SKNode, 'newCGShapePath :> Class ''CGPath ] $ void 
                  [cexp| ((typename SKShapeNode *)skNode).path = newCGShapePath |])
              }
    ; case reallyUnsafePtrEquality# shapeFillColor newShapeFillColor of
        1# -> return ()
        _  -> let newSKShapeFillColor = colorToSKColor newShapeFillColor
              in
              $(objc [ 'skNode :> ''SKNode, 'newSKShapeFillColor :> Class ''SKColor ] $ void
                [cexp| ((typename SKShapeNode *)skNode).fillColor = newSKShapeFillColor |])
    ; case reallyUnsafePtrEquality# shapeLineWidth newShapeLineWidth of
        1# -> return ()
        _  -> $(objc [ 'skNode :> ''SKNode, 'newShapeLineWidth :> ''Double{-GFloat-} ] $ void 
                [cexp| ((typename SKShapeNode *)skNode).lineWidth = newShapeLineWidth |])
    ; case reallyUnsafePtrEquality# shapeGlowWidth newShapeGlowWidth of
        1# -> return ()
        _  -> $(objc [ 'skNode :> ''SKNode, 'newShapeGlowWidth :> ''Double{-GFloat-} ] $ void 
                [cexp| ((typename SKShapeNode *)skNode).glowWidth = newShapeGlowWidth |])
    ; case reallyUnsafePtrEquality# shapeAntialiased newShapeAntialiased of
        1# -> return ()
        _  -> $(objc [ 'skNode :> ''SKNode, 'newShapeAntialiased :> ''Bool ] $ void 
                [cexp| ((typename SKShapeNode *)skNode).antialiased = newShapeAntialiased |])
    ; case reallyUnsafePtrEquality# shapeStrokeColor newShapeStrokeColor of
        1# -> return ()
        _  -> let newSKShapeStrokeColor = colorToSKColor newShapeStrokeColor
              in
              $(objc [ 'skNode :> ''SKNode, 'newSKShapeStrokeColor :> Class ''SKColor ] $ void
                [cexp| ((typename SKShapeNode *)skNode).strokeColor = newSKShapeStrokeColor |])
    ; return skNode
    }
mergeSKNode Sprite { nodeForeign            = Just skNode, ..} 
            Sprite { nodeName               = newNodeName
                   , nodePosition           = newNodePosition
                   , nodeZPosition          = newNodeZPosition
                   , nodeXScale             = newNodeXScale
                   , nodeYScale             = newNodeYScale
                   , nodeZRotation          = newNodeZRotation
                   , nodeChildren           = newNodeChildren
                   , nodeActionDirectives   = newNodeActionDirectives
                   , nodeSpeed              = newNodeSpeed
                   , nodePaused             = newNodePaused
                   , nodeUserData           = newNodeUserData
                   , nodeForeign            = Just newSKNode
                   , spriteSize             = newSpriteSize
                   , spriteAnchorPoint      = newSpriteAnchorPoint
                   , spriteTexture          = newSpriteTexture
                   -- , spriteCenterRect       = newSpriteCenterRect
                   , spriteColorBlendFactor = newSpriteColorBlendFactor
                   , spriteColor            = newSpriteColor
                   }
  | skNode == newSKNode
  = do
    { addActionDirectives skNode newNodeActionDirectives       -- Execute all new action directives

        -- For every field in the scene object, update it if it changed.
        -- NB: Superflous updates of unchanged values are benign. They only affect performance
        --     negatively, but do not alter the semantics. This is important as an intervening GC
        --     might move some of the pointers that we compare unsafely.
        --
    ; updateNodeName skNode nodeName newNodeName
    ; updateNodePosition skNode nodePosition newNodePosition
    ; updateZPosition skNode nodeZPosition newNodeZPosition
    ; updateXScale skNode nodeXScale newNodeXScale
    ; updateYScale skNode nodeYScale newNodeYScale
    ; updateZRotation skNode nodeZRotation newNodeZRotation
    ; updateChildren skNode nodeChildren newNodeChildren
    ; updateSpeed skNode nodeSpeed newNodeSpeed
    ; updatePaused skNode nodePaused newNodePaused
    ; updateUserData skNode nodeUserData newNodeUserData
    ; case reallyUnsafePtrEquality# spriteSize newSpriteSize of
        1# -> return ()
        _  -> $(objc [ 'skNode :> ''SKNode, 'newSpriteSize :> ''Size ] $ void 
                [cexp| (((typename SKSpriteNode *)skNode).size = *newSpriteSize, free(newSpriteSize)) |])
    ; case reallyUnsafePtrEquality# spriteAnchorPoint newSpriteAnchorPoint of
        1# -> return ()
        _  -> $(objc [ 'skNode :> ''SKNode, 'newSpriteAnchorPoint :> ''Point ] $ void 
                [cexp| (((typename SKSpriteNode *)skNode).anchorPoint = *newSpriteAnchorPoint, free(newSpriteAnchorPoint)) |])
    ; case reallyUnsafePtrEquality# spriteTexture newSpriteTexture of
        1# -> return ()
        _  -> do
              { newSpriteTextureOrNil <- case newSpriteTexture of
                  Nothing            -> SKTexture <$> newForeignPtr_ nullPtr
                  Just spriteTexture -> return $ textureToSKTexture spriteTexture
              ; $(objc [ 'skNode :> ''SKNode, 'newSpriteTextureOrNil :> Class ''SKTexture ] $ void 
                  [cexp| ((typename SKSpriteNode *)skNode).texture = newSpriteTextureOrNil |])
              }
    ; case reallyUnsafePtrEquality# spriteColorBlendFactor newSpriteColorBlendFactor of
        1# -> return ()
        _  -> $(objc [ 'skNode :> ''SKNode, 'newSpriteColorBlendFactor :> ''Double{-GFloat-} ] $ void 
                [cexp| ((typename SKSpriteNode *)skNode).colorBlendFactor = newSpriteColorBlendFactor |])
    ; case reallyUnsafePtrEquality# spriteColor newSpriteColor of
        1# -> return ()
        _  -> let newSKSpriteColor = colorToSKColor newSpriteColor
              in
              $(objc [ 'skNode :> ''SKNode, 'newSKSpriteColor :> Class ''SKColor ] $ void
                [cexp| ((typename SKSpriteNode *)skNode).color = newSKSpriteColor |])
    ; return skNode
    }
mergeSKNode _oldNode newNode = nodeToSKNode newNode

updateNodeName skNode nodeName newNodeName
  = case reallyUnsafePtrEquality# nodeName newNodeName of
      1# -> return ()
      _  -> $(objc [ 'skNode :> ''SKNode, 'newNodeName :> [t| Maybe String |] ] $ void 
              [cexp| skNode.name = newNodeName |])

updateNodePosition skNode nodePosition newNodePosition
  = case reallyUnsafePtrEquality# nodePosition newNodePosition of
      1# -> return ()
      _  -> $(objc [ 'skNode :> ''SKNode, 'newNodePosition :> ''Point ] $ void 
              [cexp| (skNode.position = *newNodePosition, free(newNodePosition)) |])

updateZPosition skNode nodeZPosition newNodeZPosition
  = case reallyUnsafePtrEquality# nodeZPosition newNodeZPosition of
      1# -> return ()
      _  -> $(objc [ 'skNode :> ''SKNode, 'newNodeZPosition :> ''Double{-GFloat-} ] $ void 
              [cexp| skNode.zPosition = newNodeZPosition |])

updateXScale skNode nodeXScale newNodeXScale
  = case reallyUnsafePtrEquality# nodeXScale newNodeXScale of
      1# -> return ()
      _  -> $(objc [ 'skNode :> ''SKNode, 'newNodeXScale :> ''Double{-GFloat-} ] $ void 
              [cexp| skNode.xScale = newNodeXScale |])

updateYScale skNode nodeYScale newNodeYScale
  = case reallyUnsafePtrEquality# nodeYScale newNodeYScale of
      1# -> return ()
      _  -> $(objc [ 'skNode :> ''SKNode, 'newNodeYScale :> ''Double{-GFloat-} ] $ void 
              [cexp| skNode.yScale = newNodeYScale |])

updateZRotation skNode nodeZRotation newNodeZRotation
  = case reallyUnsafePtrEquality# nodeZRotation newNodeZRotation of
      1# -> return ()
      _  -> $(objc [ 'skNode :> ''SKNode, 'newNodeZRotation :> ''Double{-GFloat-} ] $ void 
              [cexp| skNode.zRotation = newNodeZRotation |])

updateChildren skNode nodeChildren newNodeChildren
  = case reallyUnsafePtrEquality# nodeChildren newNodeChildren of
      1# -> return ()
      _  -> addChildren False skNode newNodeChildren
            -- FIXME: Instead of blindly traversing all subtrees, we need to try to merge as many of the children
            --        as possible (i.e., those having the same origin, that is the same 'nodeForeign' reference).
            --        To do this, we might need to use the return value of 'mergeSKNode', which isn't used yet.
            -- See #334

updateSpeed skNode nodeSpeed newNodeSpeed
  = case reallyUnsafePtrEquality# nodeSpeed newNodeSpeed of
      1# -> return ()
      _  -> $(objc [ 'skNode :> ''SKNode, 'newNodeSpeed :> ''Double{-GFloat-} ] $ void 
              [cexp| skNode.speed = newNodeSpeed |])

updatePaused skNode nodePaused newNodePaused
  = case reallyUnsafePtrEquality# nodePaused newNodePaused of
      1# -> return ()
      _  -> $(objc [ 'skNode :> ''SKNode, 'newNodePaused :> ''Bool ] $ void 
              [cexp| skNode.paused = newNodePaused |])

updateUserData skNode nodeUserData newNodeUserData
  = case reallyUnsafePtrEquality# nodeUserData newNodeUserData of
      1# -> return ()
      _  -> let newNodeUserDataAny = unsafeCoerce (Box newNodeUserData) :: Box Any
            in
            $(objc [ 'skNode :> ''SKNode, 'newNodeUserDataAny :> [t| Box Any |] ] $ void 
              [cexp| ({
                if (skNode.userData)
                  [skNode.userData setObject:[StablePtrBox stablePtrBox:newNodeUserDataAny] forKey:@"haskellUserData"];
                else 
                  skNode.userData = [NSMutableDictionary dictionaryWithObject:[StablePtrBox stablePtrBox:newNodeUserDataAny]
                                                                       forKey:@"haskellUserData"];
              }) |])

objc_implementation [Typed 'runCustomAction] [cunit|

@interface CustomActionCallback()

@property (assign) typename HsStablePtr haskellCallbackPtr;

@end

@implementation CustomActionCallback

+ (instancetype)customActionCallback:(typename HsStablePtr)callbackPtr
{
  return [[CustomActionCallback alloc] initWithCallback:callbackPtr];
}

- (instancetype)initWithCallback:(typename HsStablePtr)callbackPtr
{
  self = [super init];
  if (self) {
    _haskellCallbackPtr = callbackPtr;
  }
  return self;
}

void spritekit_initialise(void);

+ (void)initialize
{
    // The Haskell code of the framework is loaded twice. Firstly, into the interpreter by 'GHCInstance', which makes sure the
    // code using language-c-inline is initialised. Secondly, the dylib is linked into this framework, and hence, the main app.
    // This second copy is initialised here.
    // NB: This usually happens in the 'HaskellScene' class, but if we only evaluate nodes in the playground, that doesn't
    //     get exercised.
  spritekit_initialise(); 
}

- (void)dealloc
{
  hs_free_stable_ptr(_haskellCallbackPtr);
}

- (void)runCustomActionWithNode:(typename SKNode *)node elapsedTime:(typename CGFloat)dt
{
  runCustomAction(self.haskellCallbackPtr, node, dt);
}

@end
|]

objc_emit

node_initialise = objc_initialise
