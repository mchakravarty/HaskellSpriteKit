{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns, EmptyDataDecls, MagicHash #-}

-- |
-- Module      : Graphics.SpriteKit.Scene
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- SpriteKit scene nodes.

module Graphics.SpriteKit.Scene (

  -- ** Scene representation
  Scene(..), SceneUpdate, EventHandler,
  
  -- ** Scene creation
  sceneWithSize,

  -- ** Marshalling functions (internal)
  sceneToSKNode, sceneToForeignPtr,

  scene_initialise
) where

  -- standard libraries
import Control.Applicative
import Control.Exception as Exc
import Data.Typeable
import Data.Maybe
import Foreign           hiding (void)
import GHC.Prim          (reallyUnsafePtrEquality#)
import System.IO.Unsafe  (unsafePerformIO)
import Unsafe.Coerce     (unsafeCoerce)

  -- friends
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Event
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Node
import Graphics.SpriteKit.PhysicsWorld
import Graphics.SpriteKit.Types

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h", "HaskellSpriteKit/StablePtrBox.h"]


-- Scene nodes
-- -----------

-- |SpriteKit scene description.
--
data Scene sceneData nodeData
  = Scene
    { sceneName             :: Maybe String   -- ^Optional scene node identifier (doesn't have to be unique)
    , sceneChildren         :: [Node nodeData]
    , sceneActionDirectives :: [SDirective (Scene sceneData nodeData) (Node nodeData)]
    , sceneSpeed            :: GFloat         -- ^Speed modifier for all actions in the entire subtree (default: 1.0)
    , sceneData             :: sceneData      -- ^Application specific information (default: uninitialised!)
    , scenePaused           :: Bool           -- ^If 'True' all actions in the entire subtree are skipped (default: 'False').
    , sceneAnchorPoint      :: Point          -- ^Point in the view’s frame that corresponds to the scene’s origin in unit
                                              -- coordinate space (default: (0, 0) == lower-left corner of the view's frame).
    , sceneSize             :: Size           -- ^Dimensions of the scene in points.
    , sceneScaleMode        :: SceneScaleMode -- ^How the scene is defined to the enclosing view (default: 'SceneScaleModeFill').
    , sceneBackgroundColor  :: Color          -- ^Background colour (default: RGBA 0.15, 0.15, 0.15, 1.0).
    
      -- Customisation of the animation loop
    , sceneUpdate           :: Maybe (SceneUpdate sceneData nodeData)
                                              -- ^Called once per frame before any other updates to the scene (default: 'Nothing')
    
      -- Scene physics
    , scenePhysicsWorld     :: PhysicsWorld sceneData nodeData
                                              -- ^Physics simulation associated with this scene (default: 'Nothing')
    
      -- Event handling
    , sceneHandleEvent      :: Maybe (EventHandler sceneData)
                                              -- ^Event handler for the scene (default: Nothing).
    }

-- |The modes that determine how the scene’s area is mapped to the view that presents it.
--
data SceneScaleMode = SceneScaleModeFill          -- ^Scale each axis independently to match the views.
                    | SceneScaleModeAspectFill    -- ^Preserve the aspect ratio and scale to just fill the entire view.
                    | SceneScaleModeAspectFit     -- ^Preserve the aspect ratio and scale to just not crop any content.
                    | SceneScaleModeResizeFill    -- ^No scaling, but automatically resize the scene to match the view.

-- |Scene update functions called before any actions are executed.
--
-- The second argument contains the current system time.
--
type SceneUpdate sceneData nodeData = Scene sceneData nodeData -> TimeInterval -> Scene sceneData nodeData
-- FIXME: We may prefer to return 'Maybe (Scene sceneData nodeData)' to make calls that don't modify anything cheaper.

-- |Event handler that given an input event and node data decides whether to handle the event and how to update the node data.
--
-- If the handler chooses not to handle the presented event, it returns 'Nothing'. In this case, the event will be forwarded to
-- the next item in the responder chain.
--
type EventHandler userData = Event -> userData -> Maybe userData


-- Scene creation
-- --------------

-- |A new scene of the given size.
--
sceneWithSize :: Size -> Scene sceneData nodeData
sceneWithSize size
  = Scene
    { sceneName             = Nothing
    , sceneChildren         = []
    , sceneActionDirectives = []
    , sceneSpeed            = 1
    , scenePaused           = False
    , sceneData             = error "Graphics.SpriteKit.Scene: uninitialised user data (Scene)"
    , sceneAnchorPoint      = Point 0 0
    , sceneSize             = size
    , sceneScaleMode        = SceneScaleModeFill
    , sceneBackgroundColor  = colorWithRGBA 0.15 0.15 0.15 1.0
    , sceneUpdate           = Nothing
    , scenePhysicsWorld     = physicsWorld
    , sceneHandleEvent      = Nothing
    }

-- FIXME: Features not yet supported:
--   * 'didChangeSize:' method
--   * 'convertPointFromView:' and 'convertPointToView:' (requires a marshalled scene; so to be efficient we need to cache the
--     native representation)
--   * 'willMoveFromView:', 'didMoveFromView:', and 'view' (requires a Haskell representation of 'SKView's)
--   * 'SKSceneDelegate' methods as callbacks
--   * 'physicsWorld'


-- Marshalling support
-- -------------------

objc_struct_marshaller 'pointToCGPoint   'cgPointToPoint
objc_struct_marshaller 'sizeToCGSize     'cgSizeToSize
objc_struct_marshaller 'vectorToCGVector 'cgVectorToVector

sceneScaleModeToSKSceneScaleMode :: SceneScaleMode -> CLong  -- actually 'NSInteger'
sceneScaleModeToSKSceneScaleMode SceneScaleModeFill       = sceneScaleModeFill
sceneScaleModeToSKSceneScaleMode SceneScaleModeAspectFill = sceneScaleModeAspectFill
sceneScaleModeToSKSceneScaleMode SceneScaleModeAspectFit  = sceneScaleModeAspectFit
sceneScaleModeToSKSceneScaleMode SceneScaleModeResizeFill = sceneScaleModeResizeFill

skSceneScaleModeToSceneScaleMode :: CLong -> SceneScaleMode
skSceneScaleModeToSceneScaleMode ssm
  | ssm == sceneScaleModeToSKSceneScaleMode SceneScaleModeFill       = SceneScaleModeFill
  | ssm == sceneScaleModeToSKSceneScaleMode SceneScaleModeAspectFill = SceneScaleModeAspectFill
  | ssm == sceneScaleModeToSKSceneScaleMode SceneScaleModeAspectFit  = SceneScaleModeAspectFit
  | ssm == sceneScaleModeToSKSceneScaleMode SceneScaleModeResizeFill = SceneScaleModeResizeFill
  | otherwise                                                        
  = error "Graphics.SpriteKit.Scene.skSceneScaleModeToSceneScaleMode: out of bounds"

-- NB: Seperate bindings to cache the results
{-# NOINLINE sceneScaleModeFill #-}
sceneScaleModeFill       = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKSceneScaleModeFill |])
{-# NOINLINE sceneScaleModeAspectFill #-}
sceneScaleModeAspectFill = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKSceneScaleModeAspectFill |])
{-# NOINLINE sceneScaleModeAspectFit #-}
sceneScaleModeAspectFit  = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKSceneScaleModeAspectFit |])
{-# NOINLINE sceneScaleModeResizeFill #-}
sceneScaleModeResizeFill = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKSceneScaleModeResizeFill |])


sceneToSKNode :: Scene sceneData nodeData -> IO SKNode
sceneToSKNode (scene@Scene{scenePhysicsWorld = PhysicsWorld{worldGravity, worldSpeed}, ..})
  = do
    { let userInteractionEnabled = isJust sceneHandleEvent
          skSceneScaleMode       = sceneScaleModeToSKSceneScaleMode sceneScaleMode
          skSceneBackgroundColor = colorToSKColor sceneBackgroundColor
          sceneAny               = unsafeCoerce scene             -- opaque data marshalled as a stable pointer
    ; node <- $(objc [ 'sceneName              :> [t| Maybe String |]
  -- FIXME: language-c-inline needs to look through type synonyms
                     , 'sceneSpeed             :> ''Double  -- should be ''GFloat
                     , 'scenePaused            :> ''Bool
                     , 'userInteractionEnabled :> ''Bool
                     , 'sceneAnchorPoint       :> ''Point
                     , 'sceneSize              :> ''Size
                     , 'skSceneScaleMode       :> ''CLong
                     , 'skSceneBackgroundColor :> Class ''SKColor
                     , 'worldGravity           :> ''Vector
                     , 'worldSpeed             :> ''Double  -- should be ''GFloat
                     , 'sceneAny               :> ''Any
                     ] $ Class ''SKNode <:
                [cexp| ({ 
                  typename HaskellScene *node       = [HaskellScene sceneWithSize:*sceneSize];
                  node.name                         = sceneName;
                  node.speed                        = sceneSpeed;
                  node.paused                       = scenePaused;
                  node.userInteractionEnabled       = userInteractionEnabled;
                  node.anchorPoint                  = *sceneAnchorPoint;
                  node.scaleMode                    = skSceneScaleMode;
                  node.backgroundColor              = skSceneBackgroundColor;
                  node.physicsWorld.gravity         = *worldGravity;
                  node.physicsWorld.speed           = worldSpeed;
                  node.physicsWorld.contactDelegate = node;
                  node.haskellScenePtr              = sceneAny;
                  (typename SKNode *)node; 
                }) |])
    ; addChildren True    node sceneChildren
    ; addActionDirectives node sceneActionDirectives
    ; return node
    }

sceneToForeignPtr :: Scene sceneData nodeData -> IO (ForeignPtr SKNode)
sceneToForeignPtr node = do { SKNode fptr <- sceneToSKNode node; return fptr }


-- Callbacks
-- ---------

keepSKNode :: SKNode -> IO SKNode
keepSKNode = return

objc_class_marshaller 'keepSKNode 'keepSKNode

keepSKPhysicsContact :: SKPhysicsContact -> IO SKPhysicsContact
keepSKPhysicsContact = return

objc_class_marshaller 'keepSKPhysicsContact 'keepSKPhysicsContact

updateForScene :: SKNode -> Any -> Double{-TimeInterval-} -> IO ()
updateForScene skNode sceneAny currentTime
  = (case sceneUpdate oldScene of
      Nothing     -> return ()
      Just update -> do
                     { -- NB: The following code takes care to avoid creating growing thunk chains.
                     ; let newScene@Scene{..} = update currentScene currentTime
                           newSceneAny        = unsafeCoerce newScene
                     ; addActionDirectives skNode sceneActionDirectives       -- Execute all new action directives
                     
                         -- For every field in the scene object, update it if it changed.
                         -- NB: Superflous updates of unchanged values are benign. They only affect performance
                         --     negatively, but do not alter the semantics. This is important as an intervening GC
                         --     might move some of the pointers that we compare unsafely.
                         --
                     ; case reallyUnsafePtrEquality# currentName sceneName of
                         1# -> return ()
                         _  -> $(objc [ 'skNode :> ''SKNode, 'sceneName :> [t| Maybe String |] ] $ void 
                                 [cexp| skNode.name = sceneName |])
                     ; updateChildren skNode currentChildren sceneChildren
                     ; case reallyUnsafePtrEquality# currentSpeed sceneSpeed of
                         1# -> return ()
                         _  -> $(objc [ 'skNode :> ''SKNode, 'sceneSpeed :> ''Double{-GFloat-} ] $ void 
                                 [cexp| skNode.speed = sceneSpeed |])
                     ; case reallyUnsafePtrEquality# currentPaused scenePaused of
                         1# -> return ()
                         _  -> $(objc [ 'skNode :> ''SKNode, 'scenePaused :> ''Bool ] $ void 
                                 [cexp| skNode.paused = scenePaused |])
                     ; case reallyUnsafePtrEquality# currentAnchorPoint sceneAnchorPoint of
                         1# -> return ()
                         _  -> $(objc [ 'skNode :> ''SKNode, 'sceneAnchorPoint :> ''Point ] $ void 
                                 [cexp| ((typename SKScene*)skNode).anchorPoint = *sceneAnchorPoint |])
                        
                         -- Only change the size if its value actually changed. Size changes are fairly expensive.
                     ; case reallyUnsafePtrEquality# currentSize sceneSize of
                         1#                            -> return ()
                         _  | currentSize == sceneSize -> return ()
                            | otherwise                -> $(objc [ 'skNode :> ''SKNode, 'sceneSize :> ''Size ] $ void 
                                                            [cexp| ((typename SKScene*)skNode).size = *sceneSize |])

                     ; case reallyUnsafePtrEquality# currentScaleMode sceneScaleMode of
                         1# -> return ()
                         _  -> let skSceneScaleMode = sceneScaleModeToSKSceneScaleMode sceneScaleMode
                               in
                               $(objc [ 'skNode :> ''SKNode, 'skSceneScaleMode :> ''CLong ] $ void 
                                 [cexp| ((typename SKScene*)skNode).scaleMode = skSceneScaleMode |])
                     ; case reallyUnsafePtrEquality# currentBackgroundColor sceneBackgroundColor of
                         1# -> return ()
                         _  -> let skSceneBackgroundColor = colorToSKColor sceneBackgroundColor
                               in
                               $(objc [ 'skNode :> ''SKNode, 'skSceneBackgroundColor :> Class ''SKColor ] $ void 
                                 [cexp| ((typename SKScene*)skNode).backgroundColor = skSceneBackgroundColor |])

                     ; case reallyUnsafePtrEquality# currentPhysicsWorld scenePhysicsWorld of
                         1# -> return ()
                         _  -> let PhysicsWorld{worldGravity, worldSpeed} = scenePhysicsWorld
                               in
                               $(objc [ 'skNode :> ''SKNode, 'worldGravity :> ''Vector, 'worldSpeed :> ''Double{-GFloat-} ] $ void 
                                  [cexp| (
                                    (void)(((typename SKScene*)skNode).physicsWorld.gravity = *worldGravity),
                                    ((typename SKScene*)skNode).physicsWorld.speed   = worldSpeed ) |])

                         -- Update the reference to the Haskell scene kept by the 'SKScene' object.
                     ; $(objc [ 'skNode :> ''SKNode, 'newSceneAny :> ''Any ] $ void 
                         [cexp| ((typename HaskellScene*)skNode).haskellScenePtr = newSceneAny |])
                     }
    ) `Exc.catch` \exc -> do
      {   -- FIXME: This error needs to go into the results table of the playground.
      ; putStrLn $ "Graphics.SpriteKit: sceneUpdate: " ++ show (exc :: Exc.SomeException)
      ; return ()
      }
  where
    oldScene     = unsafeCoerce sceneAny
    currentScene = Scene  -- NB: the fields are marshalled lazily, most of them will usually not be touched
                   { sceneName             = currentName
                   , sceneChildren         = currentChildren
                   , sceneActionDirectives = []
                   , sceneSpeed            = currentSpeed
                   , scenePaused           = currentPaused
                   , sceneData             = sceneData oldScene           -- can't have been changed by SpriteKit
                   , sceneAnchorPoint      = currentAnchorPoint
                   , sceneSize             = currentSize
                   , sceneScaleMode        = currentScaleMode
                   , sceneBackgroundColor  = currentBackgroundColor
                   , sceneUpdate           = sceneUpdate oldScene         -- can't have been changed by SpriteKit
                   , scenePhysicsWorld     = currentPhysicsWorld
                   , sceneHandleEvent      = sceneHandleEvent oldScene    -- can't have been changed by SpriteKit
                   }
    currentName            = unsafePerformIO $(objc [ 'skNode :> ''SKNode ] $ [t| Maybe String |] <: 
                                               [cexp| skNode.name |])
    currentChildren        = unsafePerformIO $ do
                             { arr <- $(objc ['skNode :> Class ''SKNode] $  Class [t| NSArray SKNode |] <: 
                                        [cexp| skNode.children |])
                             ; unsafeInterleaveNSArrayTolistOfNode arr
                             }
    currentSpeed           = unsafePerformIO $(objc [ 'skNode :> ''SKNode ] $ ''Double{-GFloat-} <: 
                                               [cexp| skNode.speed |])
    currentPaused          = unsafePerformIO $(objc [ 'skNode :> ''SKNode ] $ ''Bool <: 
                                               [cexp| skNode.paused |])
    currentAnchorPoint     = unsafePerformIO $(objc [ 'skNode :> ''SKNode ] $ ''Point <: 
                                               [cexp| ({
                                                 typename CGPoint *pnt = (typename CGPoint *) malloc(sizeof(CGPoint)); 
                                                 *pnt = ((typename SKScene*)skNode).anchorPoint;
                                                 pnt;
                                                }) |])
    currentSize            = unsafePerformIO $(objc [ 'skNode :> ''SKNode ] $ ''Size <: 
                                               [cexp| ({
                                                 typename CGSize *sz = (typename CGSize *) malloc(sizeof(CGSize)); 
                                                 *sz = ((typename SKScene*)skNode).size;
                                                 sz;
                                                }) |])
    currentScaleMode       = skSceneScaleModeToSceneScaleMode $
                               unsafePerformIO $(objc [ 'skNode :> ''SKNode ] $ ''CLong <: 
                                                 [cexp| ((typename SKScene*)skNode).scaleMode |])
    currentBackgroundColor = unsafePerformIO $ Color <$> $(objc [ 'skNode :> ''SKNode ] $ Class ''SKColor <: 
                                                           [cexp| ((typename SKScene*)skNode).backgroundColor |])
    currentPhysicsWorld    = PhysicsWorld
                             { worldGravity = unsafePerformIO $(objc [ 'skNode :> ''SKNode ] $ ''Vector <: 
                                               [cexp| ({
                                                 typename CGVector *vec = (typename CGVector *) malloc(sizeof(CGVector)); 
                                                 *vec = ((typename SKScene*)skNode).physicsWorld.gravity;
                                                 vec;
                                                }) |])
                             , worldSpeed   = unsafePerformIO $(objc [ 'skNode :> ''SKNode ] $ ''Double{-GFloat-} <: 
                                                                [cexp| ((typename SKScene*)skNode).physicsWorld.speed |])
                             , worldContactDidBegin = (worldContactDidBegin . scenePhysicsWorld) oldScene
                             , worldContactDidEnd   = (worldContactDidEnd . scenePhysicsWorld) oldScene
                                                                        -- last two can't have been changed by SpriteKit
                             }

handleContact :: SKNode -> Any -> Bool -> SKPhysicsContact -> IO ()
handleContact skNode sceneAny didBegin skContact
  = (case selectHandler didBegin (scenePhysicsWorld oldScene) of
      Nothing            -> return ()
      Just handleContact -> do
                            { let (optSceneData, optBodyA, optBodyB) = handleContact oldSceneData contact
                            ; case optSceneData of
                                Nothing           -> return ()
                                Just newSceneData -> do
                                  { let newScene    = oldScene { sceneData = newSceneData }
                                        newSceneAny = newScene `seq` unsafeCoerce newScene   -- Better not coerce thunks to 'Any'

                                      -- Update the reference to the Haskell scene kept by the 'SKScene' object.
                                  ; $(objc [ 'skNode :> ''SKNode, 'newSceneAny :> ''Any ] $ void 
                                      [cexp| ((typename HaskellScene*)skNode).haskellScenePtr = newSceneAny |])
                                  }
                            ; case optBodyA of
                                Nothing       -> return ()
                                Just newBodyA -> do { _ <- mergeSKNode (contactBodyA contact) newBodyA; return () }
                            ; case optBodyB of
                                Nothing       -> return ()
                                Just newBodyB -> do { _ <- mergeSKNode (contactBodyB contact) newBodyB; return () }
                            }
    ) `Exc.catch` \exc -> do
      {   -- FIXME: This error needs to go into the results table of the playground.
      ; putStrLn $ "Graphics.SpriteKit: handleContact: " ++ show (exc :: Exc.SomeException)
      ; return ()
      }
  where
    oldScene     = unsafeCoerce sceneAny
    oldSceneData = sceneData oldScene
    contact      = skPhysicsContactToPhysicsContact skContact

    selectHandler True  world = worldContactDidBegin world
    selectHandler False world = worldContactDidEnd world

handleEventForScene :: SKNode -> Any -> Event -> IO Bool
handleEventForScene skNode sceneAny event
  = (case sceneHandleEvent oldScene of
      Nothing          -> return False
      Just handleEvent -> case handleEvent event (sceneData oldScene) of
                            Nothing           -> return False
                            Just newSceneData -> do
                              { let newScene    = oldScene { sceneData = newSceneData }
                                    newSceneAny = newScene `seq` unsafeCoerce newScene    -- Better not coerce thunks to 'Any'.

                                  -- Update the reference to the Haskell scene kept by the 'SKScene' object.
                              ; $(objc [ 'skNode :> ''SKNode, 'newSceneAny :> ''Any ] $ void 
                                  [cexp| ((typename HaskellScene*)skNode).haskellScenePtr = newSceneAny |])
                              ; return True
                              }
    ) `Exc.catch` \exc -> do
      {   -- FIXME: This error needs to go into the results table of the playground.
      ; putStrLn $ "Graphics.SpriteKit: sceneHandleEvent: " ++ show (exc :: Exc.SomeException)
      ; return False
      }
  where
    oldScene = unsafeCoerce sceneAny

handleKeyEventForScene :: SKNode -> Any -> Point -> Double{-TimeInterval-} -> CLong -> String -> String -> Bool -> Word{-should be Word16, but language-c-inline doesn't support that yet-}
                       -> IO Bool
handleKeyEventForScene skNode sceneAny locationInNode timestamp eventType characters charactersIgnoringModifiers isARepeat keyCode
  = handleEventForScene skNode sceneAny $
      keyEvent locationInNode timestamp eventType characters charactersIgnoringModifiers isARepeat (fromIntegral keyCode)

handleMouseEventForScene :: SKNode -> Any -> Point -> Double{-TimeInterval-} -> CLong -> Int -> Int -> Int -> Float
                       -> IO Bool
handleMouseEventForScene skNode sceneAny locationInNode timestamp eventType number buttonNumber clickCount pressure
  = handleEventForScene skNode sceneAny $
      mouseEvent locationInNode timestamp eventType number buttonNumber clickCount pressure


-- SKScene subclass to implement Haskell callbacks
-- -----------------------------------------------
--
-- It also serves as the contact delegate for its physics world.

objc_interface [cunit|

@interface HaskellScene : SKScene<SKPhysicsContactDelegate>

@property (assign) typename HsStablePtr haskellScenePtr;    // Haskell-side scene representation

@end
|]

objc_implementation [ Typed 'updateForScene
                    , Typed 'handleContact
                    , Typed 'handleKeyEventForScene
                    , Typed 'handleMouseEventForScene] [cunit|

void spritekit_initialise(void);

@implementation HaskellScene

+ (void)initialize
{
    // The Haskell code of the framework is loaded twice. Firstly, into the interpreter by 'GHCInstance', which makes sure the
    // code using language-c-inline is initialised. Secondly, the dylib is linked into this framework, and hence, the main app.
    // This second copy is initialised here.
  spritekit_initialise(); 
}

- (void)dealloc
{
  hs_free_stable_ptr(_haskellScenePtr);
}

// Once per frame SpriteKit update functions.
- (void)update:(typename NSTimeInterval)currentTime
{
  updateForScene(self, self.haskellScenePtr, currentTime);
}

// Physics world contact delegate methods

- (void)didBeginContact:(typename SKPhysicsContact *)contact
{
  handleContact(self, self.haskellScenePtr, YES, contact);  
}

- (void)didEndContact:(typename SKPhysicsContact *)contact
{
  handleContact(self, self.haskellScenePtr, NO, contact);  
}


// Event handlers (OS X)

- (void)keyDown:(typename NSEvent *)event
{
  if (![self keyEvent:event]) [super keyDown:event];
}

- (void)keyUp:(typename NSEvent *)event
{
  if (![self keyEvent:event]) [super keyUp:event];
}

- (void)flagsChanged:(typename NSEvent *)event
{
  if (![self keyEvent:event]) [super flagsChanged:event];
}

- (typename BOOL)keyEvent:(typename NSEvent *)event
{
  typename CGPoint *locationInNode = malloc(sizeof(typename CGPoint));
  *locationInNode = [event locationInNode:self];
  return handleKeyEventForScene(self, self.haskellScenePtr,
                                locationInNode,
                                event.timestamp, 
                                event.type,
                                event.characters,
                                event.charactersIgnoringModifiers,
                                event.isARepeat,
                                event.keyCode);
}

- (void)mouseDown:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super mouseDown:event];
}

- (void)mouseDragged:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super mouseDragged:event];
}

- (void)mouseUp:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super mouseUp:event];
}

- (void)mouseMoved:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super mouseMoved:event];
}

- (void)rightMouseDown:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super rightMouseDown:event];
}

- (void)rightMouseDragged:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super rightMouseDragged:event];
}

- (void)rightMouseUp:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super rightMouseUp:event];
}

- (void)otherMouseDown:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super otherMouseDown:event];
}

- (void)otherMouseDragged:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super otherMouseDragged:event];
}

- (void)otherMouseUp:(typename NSEvent *)event
{
  if (![self mouseEvent:event]) [super otherMouseUp:event];
}

- (typename BOOL)mouseEvent:(typename NSEvent *)event
{
  typename CGPoint *locationInNode = malloc(sizeof(typename CGPoint));
  *locationInNode = [event locationInNode:self];
  return handleMouseEventForScene(self, self.haskellScenePtr,
                                  locationInNode,
                                  event.timestamp, 
                                  event.type,
                                  event.eventNumber,
                                  event.buttonNumber,
                                  event.clickCount,
                                  event.pressure);
}


@end
|]

objc_emit

scene_initialise = objc_initialise
