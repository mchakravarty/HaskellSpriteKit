{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Action
-- Copyright   : [2014..2016] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Geometry types and operations

module Graphics.SpriteKit.Action (

  -- ** Action directives
  runAction, runActionWithKey, removeActionForKey, removeAllActions,
  
  -- ** Animation actions
  SActionSpecification(..), ActionSpecification, SAction(..), Action, ActionTimingMode(..), ActionTimingFunction(..),
  action,
  
  -- *** Convenience functions to construct specifications of the same name
  moveBy, moveTo, moveToX, moveToY, followPath, followPathSpeed, followPathAsOffsetOrientToPath,
  followPathAsOffsetOrientToPathSpeed, rotateByAngle, rotateToAngle, rotateToAngleShortestUnitArc, speedBy, speedTo, 
  scaleBy, scaleTo, scaleXByY, scaleXToX, scaleXTo, scaleYTo, hide, unhide, fadeIn, fadeOut, fadeAlphaBy, fadeAlphaTo,
  resizeByWidthHeight, resizeToHeight, resizeToWidth, resizeToWidthHeight, 
  setTexture, setTextureResize, setNormalTexture, setNormalTextureResize,
  animateWithTexturesTimePerFrame, animateWithTextures, animateWithTexturesTimePerFrameResizeRestore,
  animateWithTexturesResizeRestore, animateWithNormalTexturesTimePerFrame, animateWithNormalTextures, 
  animateWithNormalTexturesTimePerFrameResizeRestore, animateWithNormalTexturesResizeRestore, 
  colorizeWithColorColorBlendFactor, colorizeWithColor, colorizeWithColorBlendFactor,
  applyForce, applyTorque, applyForceAtPoint, applyImpulse, applyAngularImpulse, applyImpulseAtPoint,
  changeMassTo, changeMassBy,
  playSoundFileNamedWaitForCompletion, playSoundFileNamed, removeFromParent, runActionOnChildWithName, group, groupActions,
  sequence, sequenceActions, repeatActionCount, repeatActionForever, waitForDuration,
  waitForDurationWithRange, customAction,
  
  -- ** Marshalling support (internal)
  SKAction(..), actionToSKAction,
  TimedUpdateBox(..),

  action_initialise
) where

  -- standard libraries
import Prelude          hiding (sequence)
import Data.Typeable
import Foreign          hiding (void)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce    (unsafeCoerce)

  -- friends
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Path
import Graphics.SpriteKit.Texture
import Graphics.SpriteKit.Types

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h", "HaskellSpriteKit/StablePtrBox.h"]


-- Action directives
-- -----------------

-- |Initiate a new action.
--
runAction :: SAction node children -> SDirective node children
runAction action = RunAction action Nothing

-- |Initiate a new action and give it a name.
--
-- If an action with the same name is currently underway on a node that receives this action, the old action is removed first.
--
runActionWithKey :: SAction node children -> String -> SDirective node children
runActionWithKey action key = RunAction action (Just key)

-- |Instructs to remove any action with the give name.
--
removeActionForKey :: String -> SDirective node children
removeActionForKey = RemoveActionForKey

-- |Instructs to remove all actions from any node that receives this directive.
--
removeAllActions :: SDirective node children
removeAllActions = RemoveAllActions


-- Actions
-- -------

-- |Construct an action.
--
action :: SActionSpecification node children -> SAction node children
action spec
  = Action
    { actionSpecification  = spec
    , actionReversed       = False
    , actionSpeed          = 1.0
    , actionTimingMode     = ActionTimingLinear
    , actionTimingFunction = Nothing
    , actionDuration       = 0.0
    }

moveBy :: Vector -> SAction node children
moveBy = action . MoveBy

moveTo :: Point -> SAction node children
moveTo = action . MoveTo

moveToX :: GFloat -> SAction node children
moveToX = action . MoveToX

moveToY :: GFloat -> SAction node children
moveToY = action . MoveToY

followPath :: Path -> SAction node children
followPath path = action $ FollowPath path True True

followPathSpeed :: Path -> GFloat -> SAction node children
followPathSpeed path speed = action $ FollowPathSpeed path True True speed

followPathAsOffsetOrientToPath :: Path -> Bool -> Bool -> SAction node children
followPathAsOffsetOrientToPath path asOffset orientToPath = action $ FollowPath path asOffset orientToPath

followPathAsOffsetOrientToPathSpeed :: Path -> Bool -> Bool -> GFloat -> SAction node children
followPathAsOffsetOrientToPathSpeed path asOffset orientToPath speed = action $ FollowPathSpeed path asOffset orientToPath speed

rotateByAngle :: GFloat -> SAction node children
rotateByAngle = action . RotateByAngle

rotateToAngle :: GFloat -> SAction node children
rotateToAngle = action . RotateToAngle

rotateToAngleShortestUnitArc :: GFloat -> Bool -> SAction node children
rotateToAngleShortestUnitArc angle shortestUnitArc = action $ RotateToAngleShortestUnitArc angle shortestUnitArc

speedBy :: GFloat -> SAction node children
speedBy = action . SpeedBy

speedTo :: GFloat -> SAction node children
speedTo = action . SpeedTo

scaleBy :: GFloat -> SAction node children
scaleBy scale = action $ ScaleBy scale scale

scaleTo :: GFloat -> SAction node children
scaleTo scale = action $ ScaleTo scale scale

scaleXByY :: GFloat -> GFloat -> SAction node children
scaleXByY xScale yScale = action $ ScaleBy xScale yScale

scaleXToX :: GFloat -> GFloat -> SAction node children
scaleXToX xScale yScale = action $ ScaleTo xScale yScale

scaleXTo :: GFloat -> SAction node children
scaleXTo xScale = action $ ScaleXTo xScale

scaleYTo :: GFloat -> SAction node children
scaleYTo yScale = action $ ScaleYTo yScale

unhide :: SAction node children
unhide = action Unhide

hide :: SAction node children
hide = action Hide

fadeIn :: SAction node children
fadeIn = action FadeIn

fadeOut :: SAction node children
fadeOut = action FadeOut

fadeAlphaBy :: GFloat -> SAction node children
fadeAlphaBy = action . FadeAlphaBy

fadeAlphaTo :: GFloat -> SAction node children
fadeAlphaTo = action . FadeAlphaTo

resizeByWidthHeight :: GFloat -> GFloat -> SAction node children
resizeByWidthHeight width height = action $ ResizeByWidthHeight width height

resizeToHeight :: GFloat -> SAction node children
resizeToHeight = action . ResizeToHeight

resizeToWidth :: GFloat -> SAction node children
resizeToWidth = action . ResizeToWidth

resizeToWidthHeight :: GFloat -> GFloat -> SAction node children
resizeToWidthHeight width height = action $ ResizeToWidthHeight width height

setTexture :: Texture -> SAction node children
setTexture tex = action $ SetTexture tex True

setTextureResize :: Texture -> Bool -> SAction node children
setTextureResize tex resize = action $ SetTexture tex resize

-- |'animateWithTextures' is a shorthand for convenience.
animateWithTexturesTimePerFrame, animateWithTextures :: [Texture] -> TimeInterval -> SAction node children
animateWithTexturesTimePerFrame texs t = action $ AnimateWithTextures texs t True True
animateWithTextures = animateWithTexturesTimePerFrame

-- |'animateWithTexturesResizeRestore' is a shorthand for convenience.
animateWithTexturesTimePerFrameResizeRestore, animateWithTexturesResizeRestore 
  :: [Texture] -> TimeInterval -> Bool -> Bool -> SAction node children
animateWithTexturesTimePerFrameResizeRestore texs t resize restore = action $ AnimateWithTextures texs t resize restore
animateWithTexturesResizeRestore = animateWithTexturesTimePerFrameResizeRestore

setNormalTexture :: Texture -> SAction node children
setNormalTexture tex = action $ SetNormalTexture tex True

setNormalTextureResize :: Texture -> Bool -> SAction node children
setNormalTextureResize tex resize = action $ SetTexture tex resize

-- |'animateWithNormalTextures' is a shorthand for convenience.
animateWithNormalTexturesTimePerFrame, animateWithNormalTextures :: [Texture] -> TimeInterval -> SAction node children
animateWithNormalTexturesTimePerFrame texs t = action $ AnimateWithNormalTextures texs t True True
animateWithNormalTextures = animateWithNormalTexturesTimePerFrame

-- |'animateWithNormalTexturesResizeRestore' is a shorthand for convenience.
animateWithNormalTexturesTimePerFrameResizeRestore, animateWithNormalTexturesResizeRestore 
  :: [Texture] -> TimeInterval -> Bool -> Bool -> SAction node children
animateWithNormalTexturesTimePerFrameResizeRestore texs t resize restore 
  = action $ AnimateWithNormalTextures texs t resize restore
animateWithNormalTexturesResizeRestore = animateWithNormalTexturesTimePerFrameResizeRestore

-- |'colorizeWithColor' is a shorthand for convenience.
colorizeWithColorColorBlendFactor, colorizeWithColor :: Color -> GFloat -> SAction node children
colorizeWithColorColorBlendFactor color blendFactor = action $ ColorizeWithColor color blendFactor
colorizeWithColor = colorizeWithColorColorBlendFactor

colorizeWithColorBlendFactor :: GFloat -> SAction node children
colorizeWithColorBlendFactor = action . ColorizeWithColorBlendFactor

applyForce :: Vector -> SAction node children
applyForce force = action . ApplyForceImpulse $ ApplyForce force Nothing

applyTorque :: GFloat -> SAction node children
applyTorque = action . ApplyForceImpulse . ApplyTorque

applyForceAtPoint :: Vector -> Point -> SAction node children
applyForceAtPoint force point = action . ApplyForceImpulse $ ApplyForce force (Just point)

applyImpulse :: Vector -> SAction node children
applyImpulse impulse = action . ApplyForceImpulse $ ApplyImpulse impulse Nothing

applyAngularImpulse :: GFloat -> SAction node children
applyAngularImpulse = action . ApplyForceImpulse . ApplyAngularImpulse

applyImpulseAtPoint :: Vector -> Point -> SAction node children
applyImpulseAtPoint impulse point = action . ApplyForceImpulse $ ApplyImpulse impulse (Just point)

changeMassTo :: GFloat -> SAction node children
changeMassTo = action . ChangeMassTo

changeMassBy :: GFloat -> SAction node children
changeMassBy = action . ChangeMassBy

-- |'playSoundFileName' is a shorthand for convenience.
playSoundFileNamedWaitForCompletion, playSoundFileNamed :: String -> Bool -> SAction node children
playSoundFileNamedWaitForCompletion fname wait = action $ PlaySoundFileNamed fname wait
playSoundFileNamed = playSoundFileNamedWaitForCompletion

removeFromParent :: SAction node children
removeFromParent = action RemoveFromParent

runActionOnChildWithName :: Action children -> String -> SAction node children
runActionOnChildWithName act childName = action $ RunActionOnChildWithName act childName

-- |'groupActions' is to be symmetric with 'sequenceActions'.
group, groupActions :: [SAction node children] -> SAction node children
group = action . Group
groupActions = group

-- |'sequenceActions' is for convenience in the face of 'Prelude.sequence'.
sequence, sequenceActions :: [SAction node children] -> SAction node children
sequence = action . Sequence
sequenceActions = sequence

repeatActionCount :: SAction node children -> Int -> SAction node children
repeatActionCount act n = action $ RepeatActionCount act n

repeatActionForever :: SAction node children -> SAction node children
repeatActionForever = action . RepeatActionForever

waitForDuration :: SAction node children
waitForDuration = action $ WaitForDuration 0

waitForDurationWithRange :: TimeInterval -> SAction node children
waitForDurationWithRange = action . WaitForDuration

-- |Perform the given update function once per frame for the given duration. If the duration is 0, the function is only
-- invoked once.
--
-- The node on which the action is run is passed to the update function and gets replaced by the updated node.
--
-- FIXME: We need to document the tree merging algorithm.
--
customAction :: TimedUpdate node -> SAction node children
customAction = action . CustomAction


-- Marshalling support
-- -------------------

-- FIXME: we need to include this somehow!!!
objc_interface [cunit|

typedef struct CGPath CGPath;
typedef struct CGPath CGMutablePath;

|]

objc_marshaller 'pointToCGPoint   'cgPointToPoint
objc_marshaller 'vectorToCGVector 'cgVectorToVector

actionTimingModeToSKActionTimingMode :: ActionTimingMode -> CLong  -- actually 'NSInteger'
actionTimingModeToSKActionTimingMode ActionTimingLinear        = actionTimingLinear
actionTimingModeToSKActionTimingMode ActionTimingEaseIn        = actionTimingEaseIn
actionTimingModeToSKActionTimingMode ActionTimingEaseOut       = actionTimingEaseOut
actionTimingModeToSKActionTimingMode ActionTimingEaseInEaseOut = actionTimingEaseInEaseOut

-- NB: Seperate bindings to cache the results
{-# NOINLINE actionTimingLinear #-}
actionTimingLinear        = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKActionTimingLinear |])
{-# NOINLINE actionTimingEaseIn #-}
actionTimingEaseIn        = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKActionTimingEaseIn |])
{-# NOINLINE actionTimingEaseOut #-}
actionTimingEaseOut       = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKActionTimingEaseOut |])
{-# NOINLINE actionTimingEaseInEaseOut #-}
actionTimingEaseInEaseOut = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKActionTimingEaseInEaseOut |])

objc_typecheck

listOfTextureToNSArray :: [Texture] -> IO (NSArray SKTexture)
listOfTextureToNSArray textures
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray SKTexture|] <: [cexp| [NSMutableArray arrayWithCapacity:20] |])
    ; mapM_ (addElement marr . textureToSKTexture) textures
    ; return $ unsafeFreezeNSMutableArray marr
    }
  where
    addElement marr texture
      = $(objc ['marr :> Class [t|NSMutableArray SKTexture|], 'texture :> Class ''SKTexture] $ void 
          [cexp| [marr addObject:texture] |])

listOfActionsToNSArray :: [SAction node children] -> IO (NSArray SKAction)
listOfActionsToNSArray actions
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray SKAction|] <: [cexp| [NSMutableArray arrayWithCapacity:10] |])
    ; mapM_ (addElement marr) actions
    ; return $ unsafeFreezeNSMutableArray marr
    }
  where
    addElement marr action
      = do
        { skAction <- actionToSKAction action
        ; $(objc ['marr :> Class [t|NSMutableArray SKAction|], 'skAction :> Class ''SKAction] $ void 
            [cexp| [marr addObject:skAction] |])
        }

actionToSKAction :: SAction node children -> IO SKAction
actionToSKAction (Action {..})
      -- NB: We cannot factorise out the common code without spreading it across modules due to GHC's staging restriction.
  = let skActionTimingMode = actionTimingModeToSKActionTimingMode actionTimingMode
    in case actionSpecification of
      MoveBy vec
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'vec                  :> ''Vector
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction moveBy:*vec duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               free(vec);
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      MoveTo pnt
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'pnt                  :> ''Point
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction moveTo:*pnt duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               free(pnt);
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      MoveToX x
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'x                    :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction moveToX:x duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      MoveToY y
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'y                    :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction moveToY:y duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      FollowPath path asOffset orientToPath
        -> do 
           { cgPath <- pathToCGPath path
           ; $(objc [ 'actionReversed       :> ''Bool
                    , 'actionSpeed          :> ''Double  -- should be ''GFloat
                    , 'skActionTimingMode   :> ''CLong
                    -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                    , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                    , 'cgPath               :> Class ''CGPath
                    , 'asOffset             :> ''Bool
                    , 'orientToPath         :> ''Bool
                    ] $ Class ''SKAction <:
               [cexp| ({ 
                 typename SKAction *action = [SKAction followPath:cgPath 
                                                         asOffset:asOffset 
                                                     orientToPath:orientToPath 
                                                         duration:actionDuration];
                 action.speed              = actionSpeed;
                 action.timingMode         = skActionTimingMode;
  // FIXME       action.timingFunction     = actionTimingFunction;
                 (actionReversed) ? [action reversedAction] : action;
               }) |])
           }
      FollowPathSpeed path asOffset orientToPath speed      -- NB: OS X 10.10+ & iOS 8+
        -> do 
           { cgPath <- pathToCGPath path
           ; $(objc [ 'actionReversed       :> ''Bool
                    , 'actionSpeed          :> ''Double  -- should be ''GFloat
                    , 'skActionTimingMode   :> ''CLong
                    -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                    -- NB: We don't need the duration in this case.
                    , 'cgPath               :> Class ''CGPath
                    , 'asOffset             :> ''Bool
                    , 'orientToPath         :> ''Bool
                    , 'speed                :> ''Double  -- should be ''GFloat
                    ] $ Class ''SKAction <:
               [cexp| ({ 
                 typename SKAction *action = [SKAction followPath:cgPath 
                                                         asOffset:asOffset 
                                                     orientToPath:orientToPath 
                                                            speed:speed];
                 action.speed              = actionSpeed;
                 action.timingMode         = skActionTimingMode;
  // FIXME       action.timingFunction     = actionTimingFunction;
                 (actionReversed) ? [action reversedAction] : action;
               }) |])
           }
      RotateByAngle angle
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'angle                :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction rotateByAngle:angle duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      RotateToAngle angle
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'angle                :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction rotateToAngle:angle duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      RotateToAngleShortestUnitArc angle shortestUnitArc
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'angle                :> ''Double  -- should be ''GFloat
                  , 'shortestUnitArc      :> ''Bool
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction rotateToAngle:angle duration:actionDuration shortestUnitArc:shortestUnitArc];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      SpeedBy speed
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'speed                :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction speedBy:speed duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      SpeedTo speed
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'speed                :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction speedTo:speed duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ScaleBy xScale yScale
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'xScale               :> ''Double  -- should be ''GFloat
                  , 'yScale               :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = (xScale == yScale) ? [SKAction scaleBy:xScale duration:actionDuration]
                                                              : [SKAction scaleXBy:xScale y:yScale duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ScaleTo xScale yScale
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'xScale               :> ''Double  -- should be ''GFloat
                  , 'yScale               :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = (xScale == yScale) ? [SKAction scaleTo:xScale duration:actionDuration]
                                                              : [SKAction scaleXTo:xScale y:yScale duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ScaleXTo scale
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'scale                :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction scaleXTo:scale duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ScaleYTo scale
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'scale                :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction scaleYTo:scale duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      Unhide                                                -- NB: OS X 10.10+ & iOS 8+
        -> $(objc [ 'actionReversed :> ''Bool
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction unhide];
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      Hide                                                  -- NB: OS X 10.10+ & iOS 8+
        -> $(objc [ 'actionReversed :> ''Bool
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction hide];
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      FadeIn
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction fadeInWithDuration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      FadeOut
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction fadeOutWithDuration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      FadeAlphaBy factor
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'factor               :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction fadeAlphaBy:factor duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      FadeAlphaTo factor
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'factor               :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction fadeAlphaTo:factor duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ResizeByWidthHeight width height
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'width                :> ''Double  -- should be ''GFloat
                  , 'height               :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction resizeByWidth:width height:height duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ResizeToHeight height
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'height               :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction resizeToHeight:height duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ResizeToWidth width
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'width               :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction resizeToWidth:width duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ResizeToWidthHeight width height
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'width                :> ''Double  -- should be ''GFloat
                  , 'height               :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction resizeToWidth:width height:height duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      SetTexture texture resize         -- NB: *without* resizing only OS X 10.10+ & iOS 7.1+
        -> let skTexture = textureToSKTexture texture
           in
           $(objc [ 'actionReversed :> ''Bool
                  , 'skTexture      :> Class ''SKTexture
                  , 'resize         :> ''Bool
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = (!resize) ? [SKAction setTexture:skTexture resize:resize]
                                                     : [SKAction setTexture:skTexture];     // backwards compatible
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      AnimateWithTextures textures timePerFrame resize restore
        -> do 
           { skTextures <- listOfTextureToNSArray textures
           ; $(objc [ 'actionReversed :> ''Bool
                    , 'skTextures     :> Class [t|NSArray SKTexture|]
                    , 'timePerFrame   :> ''Double  -- should be ''NSTimeInterval
                    , 'resize         :> ''Bool
                    , 'restore        :> ''Bool
                    ] $ Class ''SKAction <:
               [cexp| ({ 
                 typename SKAction *action = [SKAction animateWithTextures:skTextures 
                                                              timePerFrame:timePerFrame 
                                                                    resize:resize 
                                                                   restore:restore];
                 (actionReversed) ? [action reversedAction] : action;
               }) |])
           }
      SetNormalTexture texture resize
        -> let skTexture = textureToSKTexture texture
           in
           $(objc [ 'actionReversed :> ''Bool
                  , 'skTexture      :> Class ''SKTexture
                  , 'resize         :> ''Bool
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction setNormalTexture:skTexture resize:resize];
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      AnimateWithNormalTextures textures timePerFrame resize restore
        -> do 
           { skTextures <- listOfTextureToNSArray textures
           ; $(objc [ 'actionReversed :> ''Bool
                    , 'skTextures     :> Class [t|NSArray SKTexture|]
                    , 'timePerFrame   :> ''Double  -- should be ''NSTimeInterval
                    , 'resize         :> ''Bool
                    , 'restore        :> ''Bool
                    ] $ Class ''SKAction <:
               [cexp| ({ 
                 typename SKAction *action = [SKAction animateWithNormalTextures:skTextures 
                                                                    timePerFrame:timePerFrame 
                                                                          resize:resize 
                                                                         restore:restore];
                 (actionReversed) ? [action reversedAction] : action;
               }) |])
           }
      ColorizeWithColor color blendFactor
        -> let skColor = colorToSKColor color
           in
           $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'skColor              :> Class ''SKColor
                  , 'blendFactor          :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction colorizeWithColor:skColor 
                                                      colorBlendFactor:blendFactor 
                                                              duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ColorizeWithColorBlendFactor blendFactor
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''TimeInterval
                  , 'blendFactor          :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction colorizeWithColorBlendFactor:blendFactor duration:actionDuration];
               action.speed              = actionSpeed;
               action.timingMode         = skActionTimingMode;
// FIXME       action.timingFunction     = actionTimingFunction;
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      ApplyForceImpulse (ApplyForce vec oPnt)
              -> $(objc [ 'actionReversed       :> ''Bool
                        , 'actionSpeed          :> ''Double  -- should be ''GFloat
                        , 'skActionTimingMode   :> ''CLong
                        -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                        , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                        , 'vec                  :> ''Vector
                        , 'oPnt                 :> [t| Maybe Point |]
                        ] $ Class ''SKAction <:
                   [cexp| ({ 
                     typename SKAction *action = (oPnt) ? [SKAction applyForce:*vec atPoint:*oPnt duration:actionDuration]
                                                        : [SKAction applyForce:*vec duration:actionDuration];
                     action.speed              = actionSpeed;
                     action.timingMode         = skActionTimingMode;
      // FIXME       action.timingFunction     = actionTimingFunction;
                     free(vec);
                     free(oPnt);
                     (actionReversed) ? [action reversedAction] : action;
                   }) |])
      ApplyForceImpulse (ApplyTorque torque)
              -> $(objc [ 'actionReversed       :> ''Bool
                        , 'actionSpeed          :> ''Double  -- should be ''GFloat
                        , 'skActionTimingMode   :> ''CLong
                        -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                        , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                        , 'torque               :> ''Double  -- should be ''GFloat
                        ] $ Class ''SKAction <:
                   [cexp| ({ 
                     typename SKAction *action = [SKAction applyTorque:torque duration:actionDuration];
                     action.speed              = actionSpeed;
                     action.timingMode         = skActionTimingMode;
      // FIXME       action.timingFunction     = actionTimingFunction;
                     (actionReversed) ? [action reversedAction] : action;
                   }) |])
      ApplyForceImpulse (ApplyImpulse vec oPnt)
              -> $(objc [ 'actionReversed       :> ''Bool
                        , 'actionSpeed          :> ''Double  -- should be ''GFloat
                        , 'skActionTimingMode   :> ''CLong
                        -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                        , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                        , 'vec                  :> ''Vector
                        , 'oPnt                 :> [t| Maybe Point |]
                        ] $ Class ''SKAction <:
                   [cexp| ({ 
                     typename SKAction *action = (oPnt) ? [SKAction applyImpulse:*vec atPoint:*oPnt duration:actionDuration]
                                                        : [SKAction applyImpulse:*vec duration:actionDuration];
                     action.speed              = actionSpeed;
                     action.timingMode         = skActionTimingMode;
      // FIXME       action.timingFunction     = actionTimingFunction;
                     free(vec);
                     free(oPnt);
                     (actionReversed) ? [action reversedAction] : action;
                   }) |])
      ApplyForceImpulse (ApplyAngularImpulse impulse)
              -> $(objc [ 'actionReversed       :> ''Bool
                        , 'actionSpeed          :> ''Double  -- should be ''GFloat
                        , 'skActionTimingMode   :> ''CLong
                        -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                        , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                        , 'impulse              :> ''Double  -- should be ''GFloat
                        ] $ Class ''SKAction <:
                   [cexp| ({ 
                     typename SKAction *action = [SKAction applyAngularImpulse:impulse duration:actionDuration];
                     action.speed              = actionSpeed;
                     action.timingMode         = skActionTimingMode;
      // FIXME       action.timingFunction     = actionTimingFunction;
                     (actionReversed) ? [action reversedAction] : action;
                   }) |])
      ChangeMassTo mass
              -> $(objc [ 'actionReversed       :> ''Bool
                        , 'actionSpeed          :> ''Double  -- should be ''GFloat
                        , 'skActionTimingMode   :> ''CLong
                        -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                        , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                        , 'mass                 :> ''Double  -- should be ''GFloat
                        ] $ Class ''SKAction <:
                   [cexp| ({ 
                     typename SKAction *action = [SKAction changeMassTo:mass duration:actionDuration];
                     action.speed              = actionSpeed;
                     action.timingMode         = skActionTimingMode;
      // FIXME       action.timingFunction     = actionTimingFunction;
                     (actionReversed) ? [action reversedAction] : action;
                   }) |])
      ChangeMassBy mass
              -> $(objc [ 'actionReversed       :> ''Bool
                        , 'actionSpeed          :> ''Double  -- should be ''GFloat
                        , 'skActionTimingMode   :> ''CLong
                        -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                        , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                        , 'mass                 :> ''Double  -- should be ''GFloat
                        ] $ Class ''SKAction <:
                   [cexp| ({ 
                     typename SKAction *action = [SKAction changeMassBy:mass duration:actionDuration];
                     action.speed              = actionSpeed;
                     action.timingMode         = skActionTimingMode;
      // FIXME       action.timingFunction     = actionTimingFunction;
                     (actionReversed) ? [action reversedAction] : action;
                   }) |])
      PlaySoundFileNamed soundFile waitForCompletion
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'soundFile            :> ''String
                  , 'waitForCompletion    :> ''Bool
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction playSoundFileNamed:soundFile waitForCompletion:waitForCompletion]; 
               (actionReversed) ? [action reversedAction] : action;
             }) |])
      RemoveFromParent
        -> $(objc [] $ Class ''SKAction <:
             [cexp| [SKAction removeFromParent] |])
      RunActionOnChildWithName childAction name
        -> do
           { skChildAction <- actionToSKAction childAction
           ; $(objc [ 'actionReversed :> ''Bool
                    , 'skChildAction  :> Class ''SKAction
                    , 'name           :> ''String
                    ] $ Class ''SKAction <:
               [cexp| ({ 
                 typename SKAction *action = [SKAction runAction:skChildAction onChildWithName:name]; 
                 (actionReversed) ? [action reversedAction] : action;
               }) |])
           }
      Group actions
        -> do
           { skActions <- listOfActionsToNSArray actions
           ; $(objc [ 'actionReversed     :> ''Bool
                    , 'actionSpeed        :> ''Double  -- should be ''GFloat
                    , 'skActionTimingMode :> ''CLong
                    -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                    , 'skActions          :> Class [t| NSArray SKAction |]
                  ] $ Class ''SKAction <:
               [cexp| ({ 
                 typename SKAction *action = [SKAction group:skActions];
                 action.speed              = actionSpeed;
                 action.timingMode         = skActionTimingMode;
  // FIXME       action.timingFunction     = actionTimingFunction;
                 (actionReversed) ? [action reversedAction] : action;
               }) |])
           }
      Sequence actions
        -> do
           { skActions <- listOfActionsToNSArray actions
           ; $(objc [ 'actionReversed     :> ''Bool
                    , 'actionSpeed        :> ''Double  -- should be ''GFloat
                    , 'skActionTimingMode :> ''CLong
                    -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                    , 'skActions          :> Class [t| NSArray SKAction |]
                  ] $ Class ''SKAction <:
               [cexp| ({ 
                 typename SKAction *action = [SKAction sequence:skActions];
                 action.speed              = actionSpeed;
                 action.timingMode         = skActionTimingMode;
  // FIXME       action.timingFunction     = actionTimingFunction;
                 (actionReversed) ? [action reversedAction] : action;
               }) |])
           }
      RepeatActionCount childAction count
        -> do
           { skChildAction <- actionToSKAction childAction
           ; $(objc [ 'actionReversed :> ''Bool
                    , 'actionSpeed    :> ''Double  -- should be ''GFloat
                    , 'skChildAction  :> Class ''SKAction
                    , 'count          :> ''Int
                    ] $ Class ''SKAction <:
               [cexp| ({ 
                 typename SKAction *action = [SKAction repeatAction:skChildAction count:count]; 
                 action.speed              = actionSpeed;
                 (actionReversed) ? [action reversedAction] : action;
               }) |])
           }
      RepeatActionForever childAction
        -> do
           { skChildAction <- actionToSKAction childAction
           ; $(objc [ 'actionReversed :> ''Bool
                    , 'actionSpeed    :> ''Double  -- should be ''GFloat
                    , 'skChildAction  :> Class ''SKAction
                    ] $ Class ''SKAction <:
               [cexp| ({ 
                 typename SKAction *action = [SKAction repeatActionForever:skChildAction]; 
                 action.speed              = actionSpeed;
                 (actionReversed) ? [action reversedAction] : action;
               }) |])
           }
      WaitForDuration range
        -> $(objc [ 'actionSpeed    :> ''Double  -- should be ''GFloat
                  , 'actionDuration :> ''Double  -- should be ''TimeInterval
                  , 'range          :> ''Double  -- should be ''TimeInterval
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = (range == 0) ? [SKAction waitForDuration:actionDuration] 
                                                        : [SKAction waitForDuration:actionDuration withRange:range]; 
               action.speed              = actionSpeed;     // not sure whether that has any effect
               action;
             }) |])
      CustomAction customAction
        -> let customActionAny = unsafeCoerce (TimedUpdateBox customAction)  -- boxed up function marshalled as a stable pointer
           in
           $(objc [ 'actionDuration  :> ''Double  -- should be ''TimeInterval
                  , 'customActionAny :> [t| TimedUpdateBox Any|]
                  ] $ Class ''SKAction <:
             [cexp| ({
               typename CustomActionCallback *callback = [CustomActionCallback customActionCallback:customActionAny];
               [SKAction customActionWithDuration:actionDuration 
                                      actionBlock:^(typename SKNode *node, 
                                                    typename CGFloat elapsedTime){ 
                                        [callback runCustomActionWithNode:node elapsedTime:elapsedTime];
                                      }];
             }) |])


-- Custom action wrapper for Haskell callback
-- ------------------------------------------
--
-- We need to wrap the Haskell callback to be able to deallocate the callback's stable pointer once ObjC land doesn't
-- need it anymore. We could use the 'StablePtrBox' for that. However, then the Haksell code building the custom action
-- would beed to be able to directly refer to the 'skNodeToNode' marshalling, which would lead to a cyclic import
-- dependency. We solve this by putting the implementation of 'CustomActionCallback' into 'Node.hs' — i.e., the below
-- interface serves as a forward reference.

-- We need to wrap the 'TimedUpdate node' function into a data box before coercing to 'Any' and making a stable pointer
-- Otherwise, applying the unwrapped function, leads to a crash in Haskell RTS land.
--
data TimedUpdateBox node = TimedUpdateBox (TimedUpdate node)
  deriving Typeable
  -- FIXME: We could just use 'Box a'...

objc_interface [cunit|

@interface CustomActionCallback : NSObject

/// Create a callback object. The 'callbackPtr' is a StablePtr referring to a 'TimedUpdateBox node' value
/// after (the latter) was cast to ''TimedUpdateBox Any' (to avoid a polymorphic type in a marshalled value).
///
+ (instancetype)customActionCallback:(typename HsStablePtr)callbackPtr;

- (void)runCustomActionWithNode:(typename SKNode *)node elapsedTime:(typename CGFloat)dt;

@end
|]

objc_emit

action_initialise = objc_initialise
