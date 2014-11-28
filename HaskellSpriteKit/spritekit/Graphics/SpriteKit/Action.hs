{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Action
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Geometry types and operations

module Graphics.SpriteKit.Action (

  -- * SpriteKit actions
  ActionSpecification(..), Action(..), ActionTimingMode(..), ActionTimingFunction(..),
  action,
  
  -- ** Convenience functions to construct specifications of the same name
  moveBy, moveTo, moveToX, moveToY, followPath, followPathSpeed, followPathAsOffsetOrientToPath,
  followPathAsOffsetOrientToPathSpeed, rotateByAngle, rotateToAngle, rotateToAngleShortestUnitArc, speedBy, speedTo, 
  scaleBy, scaleTo, scaleXByY, scaleXToX, scaleXTo, scaleYTo, hide, unhide, fadeIn, fadeOut, fadeAlphaBy, fadeAlphaTo,
  resizeByWidthHeight, resizeToHeight, resizeToWidth, resizeToWidthHeight, setTexture, setTextureResize, 
  animateWithTexturesTimePerFrame, animateWithTextures, animateWithTexturesTimePerFrameResizeRestore,
  animateWithTexturesResizeRestore, colorizeWithColorColorBlendFactor, colorizeWithColor, colorizeWithColorBlendFactor,
  playSoundFileNamedWaitForCompletion, playSoundFileNamed, removeFromParent, runActionOnChildWithName, group, groupActions,
  sequence, sequenceActions, repeatActionCount, repeatActionForever, waitForDuration,
  waitForDurationWithRange, customAction,
  
  -- * Marshalling functions (internal)
  SKAction(..), actionToSKAction,

  action_initialise
) where

  -- standard libraries
import Prelude          hiding (sequence)
import Data.Typeable
import Foreign          hiding (void)
import System.IO.Unsafe (unsafePerformIO)

  -- friends
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Path
import Graphics.SpriteKit.Texture
import Graphics.SpriteKit.Types

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- |Construct an action.
--
action :: ActionSpecification userData -> Action userData
action spec
  = Action
    { actionSpecification  = spec
    , actionReversed       = False
    , actionSpeed          = 1.0
    , actionTimingMode     = ActionTimingLinear
    , actionTimingFunction = Nothing
    , actionDuration       = 0.0
    }

moveBy :: Vector -> Action userData
moveBy = action . MoveBy

moveTo :: Point -> Action userData
moveTo = action . MoveTo

moveToX :: GFloat -> Action userData
moveToX = action . MoveToX

moveToY :: GFloat -> Action userData
moveToY = action . MoveToY

followPath :: Path -> Action userData
followPath path = action $ FollowPath path True True

followPathSpeed :: Path -> GFloat -> Action userData
followPathSpeed path speed = action $ FollowPathSpeed path True True speed

followPathAsOffsetOrientToPath :: Path -> Bool -> Bool -> Action userData
followPathAsOffsetOrientToPath path asOffset orientToPath = action $ FollowPath path asOffset orientToPath

followPathAsOffsetOrientToPathSpeed :: Path -> Bool -> Bool -> GFloat -> Action userData
followPathAsOffsetOrientToPathSpeed path asOffset orientToPath speed = action $ FollowPathSpeed path asOffset orientToPath speed

rotateByAngle :: GFloat -> Action userData
rotateByAngle = action . RotateByAngle

rotateToAngle :: GFloat -> Action userData
rotateToAngle = action . RotateToAngle

rotateToAngleShortestUnitArc :: GFloat -> Bool -> Action userData
rotateToAngleShortestUnitArc angle shortestUnitArc = action $ RotateToAngleShortestUnitArc angle shortestUnitArc

speedBy :: GFloat -> Action userData
speedBy = action . SpeedBy

speedTo :: GFloat -> Action userData
speedTo = action . SpeedTo

scaleBy :: GFloat -> Action userData
scaleBy scale = action $ ScaleBy scale scale

scaleTo :: GFloat -> Action userData
scaleTo scale = action $ ScaleTo scale scale

scaleXByY :: GFloat -> GFloat -> Action userData
scaleXByY xScale yScale = action $ ScaleBy xScale yScale

scaleXToX :: GFloat -> GFloat -> Action userData
scaleXToX xScale yScale = action $ ScaleTo xScale yScale

scaleXTo :: GFloat -> Action userData
scaleXTo xScale = action $ ScaleXTo xScale

scaleYTo :: GFloat -> Action userData
scaleYTo yScale = action $ ScaleYTo yScale

unhide :: Action userData
unhide = action Unhide

hide :: Action userData
hide = action Hide

fadeIn :: Action userData
fadeIn = action FadeIn

fadeOut :: Action userData
fadeOut = action FadeIn

fadeAlphaBy :: GFloat -> Action userData
fadeAlphaBy = action . FadeAlphaBy

fadeAlphaTo :: GFloat -> Action userData
fadeAlphaTo = action . FadeAlphaTo

resizeByWidthHeight :: GFloat -> GFloat -> Action userData
resizeByWidthHeight width height = action $ ResizeByWidthHeight width height

resizeToHeight :: GFloat -> Action userData
resizeToHeight = action . ResizeToHeight

resizeToWidth :: GFloat -> Action userData
resizeToWidth = action . ResizeToWidth

resizeToWidthHeight :: GFloat -> GFloat -> Action userData
resizeToWidthHeight width height = action $ ResizeToWidthHeight width height

setTexture :: Texture -> Action userData
setTexture tex = action $ SetTexture tex True

setTextureResize :: Texture -> Bool -> Action userData
setTextureResize tex resize = action $ SetTexture tex resize

-- |'animateWithTextures' is a shorthand for convenience.
animateWithTexturesTimePerFrame, animateWithTextures :: [Texture] -> TimeInterval -> Action userData
animateWithTexturesTimePerFrame texs t = action $ AnimateWithTextures texs t True True
animateWithTextures = animateWithTexturesTimePerFrame

-- |'animateWithTexturesResizeRestore' is a shorthand for convenience.
animateWithTexturesTimePerFrameResizeRestore, animateWithTexturesResizeRestore 
  :: [Texture] -> TimeInterval -> Bool -> Bool -> Action  userData
animateWithTexturesTimePerFrameResizeRestore texs t resize restore = action $ AnimateWithTextures texs t resize restore
animateWithTexturesResizeRestore = animateWithTexturesTimePerFrameResizeRestore

-- |'colorizeWithColor' is a shorthand for convenience.
colorizeWithColorColorBlendFactor, colorizeWithColor :: Color -> GFloat -> Action userData
colorizeWithColorColorBlendFactor color blendFactor = action $ ColorizeWithColor color blendFactor
colorizeWithColor = colorizeWithColorColorBlendFactor

colorizeWithColorBlendFactor :: GFloat -> Action userData
colorizeWithColorBlendFactor = action . ColorizeWithColorBlendFactor

-- |'playSoundFileName' is a shorthand for convenience.
playSoundFileNamedWaitForCompletion, playSoundFileNamed :: String -> Bool -> Action userData
playSoundFileNamedWaitForCompletion fname wait = action $ PlaySoundFileNamed fname wait
playSoundFileNamed = playSoundFileNamedWaitForCompletion

removeFromParent :: Action userData
removeFromParent = action RemoveFromParent

runActionOnChildWithName :: Action userData -> String -> Action userData
runActionOnChildWithName act childName = action $ RunActionOnChildWithName act childName

-- |'groupActions' is to be symmetric with 'sequenceActions'.
group, groupActions :: [Action userData] -> Action userData
group = action . Group
groupActions = group

-- |'sequenceActions' is for convenience in the face of 'Prelude.sequence'.
sequence, sequenceActions :: [Action userData] -> Action userData
sequence = action . Sequence
sequenceActions = sequence

repeatActionCount :: Action userData -> Int -> Action userData
repeatActionCount act n = action $ RepeatActionCount act n

repeatActionForever :: Action userData -> Action userData
repeatActionForever = action . RepeatActionForever

waitForDuration :: Action userData
waitForDuration = action $ WaitForDuration 0

waitForDurationWithRange :: TimeInterval -> Action userData
waitForDurationWithRange = action . WaitForDuration

-- |Perform the given update function once per frame for the given duration. If the duration is 0, the function is only
-- invoked once.
--
-- The node on which the action is run is passed to the update function and gets replaced by the updated node.
--
-- FIXME: We need to document the tree merging algorithm.
--
customAction :: NodeUpdate userData -> Action userData
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

actionTimingLinear        = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKActionTimingLinear |])
actionTimingEaseIn        = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKActionTimingEaseIn |])
actionTimingEaseOut       = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKActionTimingEaseOut |])
actionTimingEaseInEaseOut = unsafePerformIO $(objc [] $ ''CLong <: [cexp| SKActionTimingEaseInEaseOut |])

newtype SKAction = SKAction (ForeignPtr SKAction)
  deriving Typeable   -- needed for now until migrating to new TH

newtype NSMutableArray e = NSMutableArray (ForeignPtr (NSMutableArray e))
  deriving Typeable   -- needed for now until migrating to new TH
newtype NSArray        e = NSArray        (ForeignPtr (NSArray        e))
  deriving Typeable   -- needed for now until migrating to new TH

unsafeFreezeNSMutableArray :: NSMutableArray e -> NSArray e
unsafeFreezeNSMutableArray (NSMutableArray fptr) = NSArray $ castForeignPtr fptr

objc_typecheck

listOfTextureToNSArray :: [Texture] -> IO (NSArray SKTexture)
listOfTextureToNSArray textures
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray SKTexture|] <: [cexp| [NSMutableArray arrayWithCapacity:20] |])
    ; mapM_ (addElement marr) textures
    ; return $ unsafeFreezeNSMutableArray marr
    }
  where
    addElement marr texture
      = $(objc ['marr :> Class [t|NSMutableArray SKTexture|], 'texture :> Class ''SKTexture] $ void 
          [cexp| [marr addObject:texture] |])

listOfActionsToNSArray :: [Action userData] -> IO (NSArray SKAction)
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

actionToSKAction :: Action userData -> IO SKAction
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
        -> $(objc [ 'actionReversed :> ''Bool
                  , 'texture        :> Class ''SKTexture
                  , 'resize         :> ''Bool
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = (!resize) ? [SKAction setTexture:texture resize:resize]
                                                     : [SKAction setTexture:texture];     // backwards compatible
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
      ColorizeWithColor color blendFactor
        -> $(objc [ 'actionReversed       :> ''Bool
                  , 'actionSpeed          :> ''Double  -- should be ''GFloat
                  , 'skActionTimingMode   :> ''CLong
                  -- , 'actionTimingFunction :> [t| Maybe ActionTimingFunction |]
                  , 'actionDuration       :> ''Double  -- should be ''NSTimeInterval
                  , 'color                :> Class ''SKColor
                  , 'blendFactor          :> ''Double  -- should be ''GFloat
                  ] $ Class ''SKAction <:
             [cexp| ({ 
               typename SKAction *action = [SKAction colorizeWithColor:color 
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
        -> error "Graphics.SpriteKit.Action: custom actions are not yet implemented"

objc_emit

action_initialise = objc_initialise
