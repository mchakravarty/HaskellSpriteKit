{-# LANGUAGE DeriveDataTypeable, RecordWildCards, EmptyDataDecls, StandaloneDeriving, PolyKinds #-}

-- |
-- Module      : Graphics.SpriteKit.Types
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Core data structures of Sprite Kit.

module Graphics.SpriteKit.Types (

  -- * Tree nodes
  Node(..), 
  
  -- * Node directives
  Directive(..),
  
  -- * Actions
  ActionSpecification(..), TimedUpdate, Action(..), ActionTimingMode(..), ActionTimingFunction,

  -- ** Internal marshalling support
  SKNode(..), SKAction(..),
  GHC.Any, Box(..), NSMutableArray(..), NSArray(..), 
  unsafeFreezeNSMutableArray
) where

  -- standard libraries
import Data.Typeable
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import qualified GHC.Prim as GHC

  -- friends
import Graphics.SpriteKit.Color
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Path
import Graphics.SpriteKit.Texture


-- Node tree
-- ---------

-- |Tree structure of SpriteKit nodes that are used to assemble scenes, parameterised by the type of user data 'u'.
--
-- FIXME: or should we factorise into a two-level structure? (but that would make it awkward to use record updates)
data Node u
  = Node
    { nodeName               :: Maybe String  -- ^Optional node identifier (doesn't have to be unique)
    , nodePosition           :: Point         -- ^The position of the node in its parent's coordinate system.
    , nodeZPosition          :: GFloat        -- ^The height of the node relative to its parent (default: 0.0)
    , nodeXScale             :: GFloat        -- ^Scaling factor multiplying the width of a node and its children (default: 1.0)
    , nodeYScale             :: GFloat        -- ^Scaling factor multiplying the height of a node and its children (default: 1.0)
    , nodeZRotation          :: GFloat        -- ^Euler rotation about the z axis (in radians; default: 0.0)
    , nodeChildren           :: [Node u]
    , nodeActionDirectives   :: [Directive (Node u)]
    , nodeSpeed              :: GFloat        -- ^Speed modifier for all actions in the entire subtree (default: 1.0)
    , nodePaused             :: Bool          -- ^If 'True' all actions in the entire subtree are skipped (default: 'False').
    , nodeUserData           :: u             -- ^Application specific information (default: uninitialised!)
    , nodeForeign            :: Maybe SKNode  -- ^Internal
    }
  | Label
    { nodeName               :: Maybe String  -- ^Optional node identifier (doesn't have to be unique)
    , nodePosition           :: Point         -- ^The position of the node in its parent's coordinate system.
    , nodeZPosition          :: GFloat        -- ^The height of the node relative to its parent (default: 0.0)
    , nodeXScale             :: GFloat        -- ^Scaling factor multiplying the width of a node and its children (default: 1.0)
    , nodeYScale             :: GFloat        -- ^Scaling factor multiplying the height of a node and its children (default: 1.0)
    , nodeZRotation          :: GFloat        -- ^Euler rotation about the z axis (in radians; default: 0.0)
    , nodeChildren           :: [Node u]
    , nodeActionDirectives   :: [Directive (Node u)]
    , nodeSpeed              :: GFloat        -- ^Speed modifier for all actions in the entire subtree (default: 1.0)
    , nodePaused             :: Bool          -- ^If 'True' all actions in the entire subtree are skipped (default: 'False').
    , nodeUserData           :: u             -- ^Application specific information
    , nodeForeign            :: Maybe SKNode  -- ^Internal
    , labelText              :: String        -- ^Text displayed by the node.
    , labelFontColor         :: Color         -- ^The colour of the label (default: white).
    , labelFontName          :: Maybe String  -- ^The font used for the label.
    , labelFontSize          :: GFloat        -- ^The size of the font used in the label (default: 32pt).
    }  
  | Shape
    { nodeName               :: Maybe String  -- ^Optional node identifier (doesn't have to be unique)
    , nodePosition           :: Point         -- ^The position of the node in its parent's coordinate system.
    , nodeZPosition          :: GFloat        -- ^The height of the node relative to its parent (default: 0.0)
    , nodeXScale             :: GFloat        -- ^Scaling factor multiplying the width of a node and its children (default: 1.0)
    , nodeYScale             :: GFloat        -- ^Scaling factor multiplying the height of a node and its children (default: 1.0)
    , nodeZRotation          :: GFloat        -- ^Euler rotation about the z axis (in radians; default: 0.0)
    , nodeChildren           :: [Node u]
    , nodeActionDirectives   :: [Directive (Node u)]
    , nodeSpeed              :: GFloat        -- ^Speed modifier for all actions in the entire subtree (default: 1.0)
    , nodePaused             :: Bool          -- ^If 'True' all actions in the entire subtree are skipped (default: 'False').
    , nodeUserData           :: u             -- ^Application specific information
    , nodeForeign            :: Maybe SKNode  -- ^Internal
    , shapePath              :: Path          -- ^Graphics path as a series of shapes or lines.
    , shapeFillColor         :: Color         -- ^The color used to fill the shape (default: clear == not filled).
    , shapeLineWidth         :: GFloat        -- ^The width used to stroke the path (default: 1.0; should be <= 2.0).
    , shapeGlowWidth         :: GFloat        -- ^Glow extending outward from the stroked line (default: 0.0 == no glow).
    , shapeAntialiased       :: Bool          -- ^Smooth stroked path during drawing? (default: True).
    , shapeStrokeColor       :: Color         -- ^Colour used to stroke the shape (default: white; clear == no stroke).
    }
  | Sprite 
    { nodeName               :: Maybe String  -- ^Optional node identifier (doesn't have to be unique)
    , nodePosition           :: Point         -- ^The position of the node in its parent's coordinate system.
    , nodeZPosition          :: GFloat        -- ^The height of the node relative to its parent (default: 0.0)
    , nodeXScale             :: GFloat        -- ^Scaling factor multiplying the width of a node and its children (default: 1.0)
    , nodeYScale             :: GFloat        -- ^Scaling factor multiplying the height of a node and its children (default: 1.0)
    , nodeZRotation          :: GFloat        -- ^Euler rotation about the z axis (in radians; default: 0.0)
    , nodeChildren           :: [Node u]
    , nodeActionDirectives   :: [Directive (Node u)]
    , nodeSpeed              :: GFloat        -- ^Speed modifier for all actions in the entire subtree (default: 1.0)
    , nodePaused             :: Bool          -- ^If 'True' all actions in the entire subtree are skipped (default: 'False').
    , nodeUserData           :: u             -- ^Application specific information
    , nodeForeign            :: Maybe SKNode  -- ^Internal
    , spriteSize             :: Size          -- ^The dimensions of the sprite, in points.
    , spriteAnchorPoint      :: Point         -- ^The point in the sprite that corresponds to the node’s position.
                                              -- ^In unit coordinate space; default: (0.5,0.5); i.e., centered on its position.
    , spriteTexture          :: Maybe Texture
    -- , spriteCenterRect      :: Rect  -- FIXME: not yet supported
    , spriteColorBlendFactor :: GFloat        -- ^Default = 0 ('spriteColor' is ignored when drawing texture)
                                              -- ^value >0 means texture is blended with 'spriteColour' before being drawn
    , spriteColor            :: Color         -- ^The sprite’s color.
    } 


-- Action directives
-- -----------------

-- |Specification of changes that should be made to a node's actions.
--
data Directive node = RunAction          (Action node) (Maybe String)   -- ^Initiate a new action, possibly named.
                    | RemoveActionForKey String                         -- ^Remove a named action.
                    | RemoveAllActions                                  -- ^Remove all current actions.


-- Actions
-- -------

-- |Specification of an action that can be applied to a SpriteKit node.
--
-- Most actions will be animated over time, given a duration.
--
data ActionSpecification node

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
  | RotateToAngleShortestUnitArc      
                       GFloat Bool    -- ^Rotate to an absolute angle. If second argument '== True', in the direction resulting
                                      -- in the smallest rotation; otherwise, interpolated (irreversible).

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
  | AnimateWithTextures [Texture]     
                        TimeInterval  
                        Bool Bool     -- ^Animate through the given textures, pausing by the given time interval between textures.
                                      -- Rotate to an absolute angle. If second argument '== True', in the direction resulting
                                      -- second 'Bool' is 'True', the original texture is restored (reversible).
  | ColorizeWithColor   Color GFloat  -- ^Animate a sprite's color and blend factor (irreversible).
  | ColorizeWithColorBlendFactor 
                        GFloat        -- ^Animate a sprite's blend factor (irreversible).

      -- Field node strength animations
  -- FIXME: not yet implemented

      -- Sound animation
  | PlaySoundFileNamed  String Bool   -- ^Play a sound, maybe waiting until the sound finishes playing (irreversible).

      -- Node removal animation
  | RemoveFromParent                  -- ^Removes the animated node from its parent (irreversible; instantaneous).

      -- Action performing animation
  | RunActionOnChildWithName 
                        (Action node)
                        String        -- ^Run an action on a named child node (reversible; instantaneous).

      -- Grouping animations
  | Group               [Action node] -- ^Run all actions in the group in parallel (reversible).
  | Sequence            [Action node] -- ^Run all actions in the group in sequence (reversible).
  | RepeatActionCount   (Action node) 
                        Int           -- ^Repeat an action a fixed number of times (reversible).
  | RepeatActionForever (Action node) -- ^Repeat an action undefinitely (reversible).

      -- Animation delay
  | WaitForDuration     TimeInterval  -- ^Waits for the action's duration +/- half the given range value (irreversible).

      -- Inverse kinematic animations
  -- FIXME: not yet implemented

      -- Custom animation
  | CustomAction        (TimedUpdate node)
                                      -- ^Repeatedly invoke the update function over the action duration (irreversible).

-- |Function that computes an updated tree, given the time that elapsed since the start of the current animation.
--
-- The result will be ignored if the new node is not derived from the old node — i.e, it must be the same kind of node
-- and it must preserve the 'nodeForeign' field.
--
type TimedUpdate node = node -> GFloat -> node

-- |SpriteKit action.
--
-- NB: 'actionTimingFunction' not yet supported.
data Action node
  = Action
    { actionSpecification  :: ActionSpecification node    -- ^Determines the action to be performed.
    , actionReversed       :: Bool                        -- ^Reverses the behaviour of another action (default: 'False').
    , actionSpeed          :: GFloat                      -- ^Speed factor that modifies how fast an action runs (default: 1.0).
    , actionTimingMode     :: ActionTimingMode            -- ^Determines the action timing (default: 'ActionTimingLinear').
    , actionTimingFunction :: Maybe ActionTimingFunction  -- ^Customises the above timing mode (OS X 10.10+ & iOS 8+).
    , actionDuration       :: TimeInterval                -- ^Duration required to complete an action (default: 0.0 == immediate).
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


-- Internal marshalling support
-- ----------------------------

-- Foreign 'SKNode' reference
--
newtype SKNode = SKNode (ForeignPtr SKNode)
  deriving (Eq, 
           Typeable)   -- needed for now until migrating to new TH

-- Foreign 'SKAction' reference
--
newtype SKAction = SKAction (ForeignPtr SKAction)
  deriving Typeable   -- needed for now until migrating to new TH

-- Wrapper to lift expressions before performing an 'unsafeCoerce' to 'GHC.Any'. This helps wrapping and unwrapping
-- thunks without evaluating them.
--
data Box a = Box a
  deriving Typeable

deriving instance Typeable GHC.Any

newtype NSMutableArray e = NSMutableArray (ForeignPtr (NSMutableArray e))
  deriving Typeable   -- needed for now until migrating to new TH
newtype NSArray        e = NSArray        (ForeignPtr (NSArray        e))
  deriving Typeable   -- needed for now until migrating to new TH

unsafeFreezeNSMutableArray :: NSMutableArray e -> NSArray e
unsafeFreezeNSMutableArray (NSMutableArray fptr) = NSArray $ castForeignPtr fptr
