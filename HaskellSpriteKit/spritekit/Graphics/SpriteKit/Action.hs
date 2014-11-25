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
  playSoundFileNameWaitForCompletion, playSoundFileName, removeFromParent, runActionOnChildWithName, runAction,
  group, groupActions, sequence, sequenceActions, repeatActionCount, repeatActionForever, waitForDuration,
  waitForDurationWithRange, customAction,
  
  -- * Marshalling functions (internal)
  SKAction(..), actionToSKAction,

  action_initialise
) where

  -- standard libraries
import Prelude          hiding (sequence)
import Data.Typeable
import Foreign          hiding (void)

  -- friends
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Node
import Graphics.SpriteKit.Path
import Graphics.SpriteKit.Texture

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]


-- Actions
-- -------

-- |Specification of an action that can be applied to a SpriteKit node.
--
-- Most actions will be animated over time, given a duration.
--
data ActionSpecification

      -- Movement actions
  = MoveBy             Vector         -- ^Move relative to current position (reversible).
  | MoveTo             Point          -- ^Move to an absolute position (irreversible).
  | MoveToX            GFloat         -- ^Move horizontally to an absolute x-position (irreversible).
  | MoveToY            GFloat         -- ^Move vertically to an absolute y-position (irreversible).
  | FollowPath         Path Bool Bool -- ^Follow path, maybe use relative offsets & maybe orient according to path (reversible).
  | FollowPathSpeed    Path Bool Bool 
                       GFloat         -- ^As above, but specifying speed in points per second (reversible; OS X 10.10+ & iOS 8+).

      -- Rotation actions
  | RotateByAngle      GFloat         -- ^Rotate by a relative value, in radians (reversible).
  | RotateToAngle      GFloat         -- ^Rotate counterclockwise to an absolute angle, in radians (irreversible).
  | RotateToAngleShortestUnitArc      -- ^Rotate to an absolute angle. If second argument '== True', in the direction resulting
                       GFloat Bool    -- ^in the smallest rotation; otherwise, interpolated (irreversible).

      -- Animation speed actions
  | SpeedBy            GFloat         -- ^Changes how fast the node executes actions by a relative value (reversible).
  | SpeedTo            GFloat         -- ^Changes how fast the node executes actions to an absolute value (irreversible).
  
      -- Scaling actions
  | ScaleBy            GFloat GFloat  -- ^Relative change of x and y scale values (reversible).
  | ScaleTo            GFloat GFloat  -- ^Change x and y scale values to an absolute values (irreversible).
  | ScaleXTo           GFloat         -- ^Change x scale value to an absolute value (irreversible).
  | ScaleYTo           GFloat         -- ^Change y scale value to an absolute value (irreversible).

      -- Visibility actions
  | Unhide                            -- ^Makes a node visible (reversible; instantaneous; OS X 10.10+ & iOS 8+).
  | Hide                              -- ^Hides a node (reversible; instantaneous; OS X 10.10+ & iOS 8+).

      -- Transparency actions
  | FadeIn                            -- ^Changes the alpha value to 1.0 (reversible).
  | FadeOut                           -- ^Changes the alpha value to 0.0 (reversible).
  | FadeAlphaBy         GFloat        -- ^Relative change of the alpha value (reversible).
  | FadeAlphaTo         GFloat        -- ^Change the alpha value to an absolute value (irreversible).

      -- Sprite node content actions
  | ResizeByWidthHeight GFloat GFloat -- ^Adjust the size of a sprite (reversible).
  | ResizeToHeight      GFloat        -- ^Change height of a sprite to an absolute value (irreversible).
  | ResizeToWidth       GFloat        -- ^Change width of a sprite to an absolute value (irreversible).
  | ResizeToWidthHeight GFloat GFloat -- ^Change width and height of a sprite to an absolute value (irreversible).
  | SetTexture          Texture Bool  -- ^Change a sprite's texture, maybe resizing the sprite (irreversible; instantaneous;
                                      -- ^without resizing only OS X 10.10+ & iOS 7.1+).
  | AnimateWithTextures [Texture]     -- ^Animate through the given textures, pausing by the given time interval between textures.
                        TimeInterval  -- ^If the first 'Bool' is 'True', the sprite is resized to match each texture. If the
                        Bool Bool     -- ^second 'Bool' is 'True', the original texture is restored (reversible).
  | ColorizeWithColor   Color GFloat  -- ^Animate a sprite's color and blend factor (irreversible).
  | ColorizeWithColorBlendFactor 
                        GFloat        -- ^Animate a sprite's blend factor (irreversible).

      -- Field node strength animations
  -- FIXME: not yet implemented
  
      -- Sound animation
  | PlaySoundFileName   String Bool   -- ^Play a sound, maybe waiting until the sound finishes playing (irreversible).

      -- Node removal animation
  | RemoveFromParent                  -- ^Removes the animated node from its parent (irreversible; instantaneous).

      -- Action performing animation
  | RunAction           Action String -- ^Run an action on a named child node (reversible; instantaneous).
  
      -- Grouping animations
  | Group               [Action]      -- ^Run all actions in the group in parallel (reversible).
  | Sequence            [Action]      -- ^Run all actions in the group in sequence (reversible).
  | RepeatActionCount   Action Int    -- ^Repeat an action a fixed number of times (reversible).
  | RepeatActionForever Action        -- ^Repeat an action undefinitely (reversible).

      -- Animation delay
  | WaitForDuration     TimeInterval  -- ^Waits for the action's duration +/- half the given range value (irreversible).
  
      -- Inverse kinematic animations
  -- FIXME: not yet implemented
  
      -- Custom animation
  | CustomAction        NodeUpdate    -- ^Repeatedly invoke the update function over the action duration (irreversible).

-- |SpriteKit action.
--
-- NB: 'actionTimingFunction' not yet supported.
data Action
  = Action
    { actionSpecification  :: ActionSpecification
    , actionReversed       :: Bool                        -- ^Reverses the behaviour of another action (default: 'False').
    , actionSpeed          :: GFloat                      -- ^Speed factor that modifies how fast an action runs (default: 1.0).
    , actionTimingMode     :: ActionTimingMode            -- ^Determines the action timing (default: 'ActionTimingLinear').
    , actionTimingFunction :: Maybe ActionTimingFunction  -- ^Customises the above timing mode (OS X 10.10+ & iOS 8+).
    , actionDuration       :: TimeInterval                -- ^Duration required to complete an action (default: 0.0 = immediate).
    }

-- |Determines the temporal progression of an action.
--
data ActionTimingMode = ActionTimingLinear
                      | ActionTimingEaseIn
                      | ActionTimingEaseOut
                      | ActionTimingEaseInEaseOut
  
-- |Projects an input value between 0.0 and 1.0, inclusive, to another value between 0.0 and 1.0 to indicate the temporal
-- progression of an action. The input 0.0 must be mapped to 0.0, and 1.0 to 1.0. Inbetween those bounds, the timing function
-- can adjust the timing of the action.
--
type ActionTimingFunction = Float -> Float

-- |Construct an action.
--
action :: ActionSpecification -> Action
action spec
  = Action
    { actionSpecification  = spec
    , actionReversed       = False
    , actionSpeed          = 1.0
    , actionTimingMode     = ActionTimingLinear
    , actionTimingFunction = Nothing
    , actionDuration       = 0.0
    }

moveBy :: Vector -> Action
moveBy = action . MoveBy

moveTo :: Point -> Action
moveTo = action . MoveTo

moveToX :: GFloat -> Action
moveToX = action . MoveToX

moveToY :: GFloat -> Action
moveToY = action . MoveToY

followPath :: Path -> Action
followPath path = action $ FollowPath path True True

followPathSpeed :: Path -> GFloat -> Action
followPathSpeed path speed = action $ FollowPathSpeed path True True speed

followPathAsOffsetOrientToPath :: Path -> Bool -> Bool -> Action
followPathAsOffsetOrientToPath path asOffset orientToPath = action $ FollowPath path asOffset orientToPath

followPathAsOffsetOrientToPathSpeed :: Path -> Bool -> Bool -> GFloat -> Action
followPathAsOffsetOrientToPathSpeed path asOffset orientToPath speed = action $ FollowPathSpeed path asOffset orientToPath speed

rotateByAngle :: GFloat -> Action
rotateByAngle = action . RotateByAngle

rotateToAngle :: GFloat -> Action
rotateToAngle = action . RotateToAngle

rotateToAngleShortestUnitArc :: GFloat -> Bool -> Action
rotateToAngleShortestUnitArc angle shortestUnitArc = action $ RotateToAngleShortestUnitArc angle shortestUnitArc

speedBy :: GFloat -> Action
speedBy = action . SpeedBy

speedTo :: GFloat -> Action
speedTo = action . SpeedTo

scaleBy :: GFloat -> Action
scaleBy scale = action $ ScaleBy scale scale

scaleTo :: GFloat -> Action
scaleTo scale = action $ ScaleTo scale scale

scaleXByY :: GFloat -> GFloat -> Action
scaleXByY xScale yScale = action $ ScaleBy xScale yScale

scaleXToX :: GFloat -> GFloat -> Action
scaleXToX xScale yScale = action $ ScaleTo xScale yScale

scaleXTo :: GFloat -> Action
scaleXTo xScale = action $ ScaleXTo xScale

scaleYTo :: GFloat -> Action
scaleYTo yScale = action $ ScaleYTo yScale

unhide :: Action
unhide = action Unhide

hide :: Action
hide = action Hide

fadeIn :: Action
fadeIn = action FadeIn

fadeOut :: Action
fadeOut = action FadeIn

fadeAlphaBy :: GFloat -> Action
fadeAlphaBy = action . FadeAlphaBy

fadeAlphaTo :: GFloat -> Action
fadeAlphaTo = action . FadeAlphaTo

resizeByWidthHeight :: GFloat -> GFloat -> Action
resizeByWidthHeight width height = action $ ResizeByWidthHeight width height

resizeToHeight :: GFloat -> Action
resizeToHeight = action . ResizeToHeight

resizeToWidth :: GFloat -> Action
resizeToWidth = action . ResizeToWidth

resizeToWidthHeight :: GFloat -> GFloat -> Action
resizeToWidthHeight width height = action $ ResizeToWidthHeight width height

setTexture :: Texture -> Action
setTexture tex = action $ SetTexture tex True

setTextureResize :: Texture -> Bool -> Action
setTextureResize tex resize = action $ SetTexture tex resize

-- |'animateWithTextures' is a shorthand for convenience.
animateWithTexturesTimePerFrame, animateWithTextures :: [Texture] -> TimeInterval -> Action
animateWithTexturesTimePerFrame texs t = action $ AnimateWithTextures texs t True True
animateWithTextures = animateWithTexturesTimePerFrame

-- |'animateWithTexturesResizeRestore' is a shorthand for convenience.
animateWithTexturesTimePerFrameResizeRestore, animateWithTexturesResizeRestore 
  :: [Texture] -> TimeInterval -> Bool -> Bool -> Action
animateWithTexturesTimePerFrameResizeRestore texs t resize restore = action $ AnimateWithTextures texs t resize restore
animateWithTexturesResizeRestore = animateWithTexturesTimePerFrameResizeRestore

-- |'colorizeWithColor' is a shorthand for convenience.
colorizeWithColorColorBlendFactor, colorizeWithColor :: Color -> GFloat -> Action
colorizeWithColorColorBlendFactor color blendFactor = action $ ColorizeWithColor color blendFactor
colorizeWithColor = colorizeWithColorColorBlendFactor

colorizeWithColorBlendFactor :: GFloat -> Action
colorizeWithColorBlendFactor = action . ColorizeWithColorBlendFactor

-- |'playSoundFileName' is a shorthand for convenience.
playSoundFileNameWaitForCompletion, playSoundFileName :: String -> Bool -> Action
playSoundFileNameWaitForCompletion fname wait = action $ PlaySoundFileName fname wait
playSoundFileName = playSoundFileNameWaitForCompletion

removeFromParent :: Action
removeFromParent = action RemoveFromParent

-- |'runAction' is a shorthand for convenience.
runActionOnChildWithName, runAction :: Action -> String -> Action
runActionOnChildWithName act childName = action $ RunAction act childName
runAction = runActionOnChildWithName

-- |'groupActions' is to be symmetric with 'sequenceActions'.
group, groupActions :: [Action] -> Action
group = action . Group
groupActions = group

-- |'sequenceActions' is for convenience in the face of 'Prelude.sequence'.
sequence, sequenceActions :: [Action] -> Action
sequence = action . Sequence
sequenceActions = sequence

repeatActionCount :: Action -> Int -> Action
repeatActionCount act n = action $ RepeatActionCount act n

repeatActionForever :: Action -> Action
repeatActionForever = action . RepeatActionForever

waitForDuration :: Action
waitForDuration = action $ WaitForDuration 0

waitForDurationWithRange :: TimeInterval -> Action
waitForDurationWithRange = action . WaitForDuration

-- |Perform the given update function once per frame for the given duration. If the duration is 0, the function is only
-- invoked once.
--
-- The node on which the action is run is passed to the update function and gets replaced by the updated node.
--
-- FIXME: We need to document the tree merging algorithm.
--
customAction :: NodeUpdate -> Action
customAction = action . CustomAction


-- Marshalling support
-- -------------------

-- objc_marshaller 'pointToCGPoint 'cgPointToPoint

newtype SKAction = AKAction (ForeignPtr SKAction)
  deriving Typeable   -- needed for now until migrating to new TH


actionToSKAction :: Action -> IO SKAction
actionToSKAction = error "FIXME"


{-
objc_typecheck
pathToCGPath path
  = do
    { mutableCGPath@(CGMutablePath fptr) <- $(objc [] $ Class ''CGMutablePath <: [cexp| CGPathCreateMutable() |])
    ; mapM_ (addPathElement mutableCGPath) path
    ; return $ CGPath (castForeignPtr fptr)      -- unsafe freeze
    }
  where
    addPathElement path (MoveToPoint (Point {..}))
     -- FIXME: language-c-inline needs to look through type synonyms
     -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
      = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''Double, 'pointY :> ''Double] $ void
          [cexp| CGPathMoveToPoint(path, NULL, pointX, pointY) |])
    addPathElement path (AddLineToPoint (Point {..}))
     -- FIXME: language-c-inline needs to look through type synonyms
     -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
      = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''Double, 'pointY :> ''Double] $ void
          [cexp| CGPathAddLineToPoint(path, NULL, pointX, pointY) |])
    addPathElement path (AddQuadCurveToPoint (Point {pointX = cpx, pointY = cpy}) (Point {pointX = x, pointY = y}))
     -- FIXME: language-c-inline needs to look through type synonyms
     -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
      = $(objc ['path :> Class ''CGMutablePath, 'cpx :> ''Double, 'cpy :> ''Double, 'x :> ''Double, 'y :> ''Double] $ void
          [cexp| CGPathAddQuadCurveToPoint(path, NULL, cpx, cpy, x, y) |])
    addPathElement path (AddCurveToPoint (Point {pointX = cp1x, pointY = cp1y}) 
                                         (Point {pointX = cp2x, pointY = cp2y})
                                         (Point {pointX = x,    pointY = y}))
     -- FIXME: language-c-inline needs to look through type synonyms
     -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
      = $(objc ['path :> Class ''CGMutablePath, 'cp1x :> ''Double, 'cp1y :> ''Double, 
                                                'cp2x :> ''Double, 'cp2y :> ''Double, 
                                                'x    :> ''Double, 'y    :> ''Double] $ void
          [cexp| CGPathAddCurveToPoint(path, NULL, cp1x, cp1y, cp2x, cp2y, x, y) |])
    addPathElement path CloseSubpath
    -- FIXME: language-c-inline needs to look through type synonyms
    -- = $(objc ['path :> Class ''CGMutablePath, 'pointX :> ''GFloat, 'pointY :> ''GFloat] $ void
     = $(objc ['path :> Class ''CGMutablePath] $ void
         [cexp| CGPathCloseSubpath(path) |])
-}

objc_emit

action_initialise = objc_initialise
