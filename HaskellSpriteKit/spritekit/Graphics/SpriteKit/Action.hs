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
  playSoundFileNameWaitForCompletion, playSoundFileName, removeFromParent, runActionOnChildWithName, group, groupActions,
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
playSoundFileNameWaitForCompletion, playSoundFileName :: String -> Bool -> Action userData
playSoundFileNameWaitForCompletion fname wait = action $ PlaySoundFileName fname wait
playSoundFileName = playSoundFileNameWaitForCompletion

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

objc_typecheck

actionToSKAction :: Action userData -> IO SKAction
actionToSKAction (Action {..})
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
               action.speed            = actionSpeed;
               action.timingMode       = skActionTimingMode;
// FIXME       action.timingFunction   = actionTimingFunction;
               free(vec);
               (actionReversed) ? [action reversedAction] : action;
             }) |])

objc_emit

action_initialise = objc_initialise
