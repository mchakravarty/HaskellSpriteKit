{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls, MagicHash #-}

-- |
-- Module      : Graphics.SpriteKit.Event
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Event support for SpriteKit. Currently, only OS X events are supported.

module Graphics.SpriteKit.Event (

  -- * Event information
  Event(..), KeyEvent(..), MouseEvent(..), EnterExitEvent(..), OtherEvent(..),

  -- * Marshalling functions (internal)
  keyEvent, mouseEvent,

  event_initialise
) where

  -- standard libraries
import Data.Typeable
import Data.Word
import Foreign          hiding (void)
import GHC.Prim         (reallyUnsafePtrEquality#)
import System.IO.Unsafe (unsafePerformIO)

  -- friends
import Graphics.SpriteKit.Geometry

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "GHC/HsFFI.h"]


-- Events
-- ------

-- |Cocoa events.
--
-- NB: Not all event types are supported. Usually, because they would require functionality from other parts of Cocoa, which are
--     not supported here (yet).
--
data Event
  = KeyEvent
    { eventLocationInNode                 :: Point          -- ^Location within the SpriteKit node receiving the event.
    -- , eventModifierFlags                  :: ??            -- ^Modifier keys that were in effect at the time of the event.
    , eventTimestamp                      :: TimeInterval   -- ^Time when the event occured in seconds since system startup.
    , keyEventType                        :: KeyEvent       -- ^The specific type of key event.
    , keyEventCharacters                  :: String         -- ^Characters associated with receiving key-up or key-down events.
    , keyEventCharactersIgnoringModifiers :: String         -- ^Ditto, ignoring the effect of modifier keys, except for Shift.
    , keyEventIsARepeat                   :: Bool           -- ^Whether event is a repeat caused by the user holding a key down.
    , keyEventKeyCode                     :: Word16         -- ^Hardware-independent, virtual keycode.
    }
  | MouseEvent
    { eventLocationInNode                 :: Point          -- ^Location within the SpriteKit node receiving the event.
    -- , eventModifierFlags                  :: ??            -- ^Modifier keys that were in effect at the time of the event.
    , eventTimestamp                      :: TimeInterval   -- ^Time when the event occured in seconds since system startup.
    , mouseEventType                      :: MouseEvent     -- ^The specific type of mouse event.
    , mouseEventNumber                    :: Int            -- ^Event identifier (increasing counter of mouse events).
    , mouseEventButtonNumber              :: Int            -- ^Number of the button used in some mouse events.
    , mouseEventClickCount                :: Int            -- ^Number of mouse clicks for a mouse event
    , mouseEventPressure                  :: Float          -- ^Value from 0 through 1 indicating pressure for some devices.
    }
  | EnterExitEvent
    { eventLocationInNode                 :: Point          -- ^Location within the SpriteKit node receiving the event.
    -- , eventModifierFlags                  :: ??            -- ^Modifier keys that were in effect at the time of the event.
    , eventTimestamp                      :: TimeInterval   -- ^Time when the event occured in seconds since system startup.
    , enterExitEventType                  :: EnterExitEvent -- ^The specific type of a tracking-rectangle or cursor-update event.
    , enterExitEventNumber                :: Int            -- ^Event identifier (increasing counter of mouse events).
    -- , enterExitTrackingNumber             :: Int
    -- , enterExitUserData                   :: ??
    }
  | OtherEvent
    { eventLocationInNode                 :: Point          -- ^Location within the SpriteKit node receiving the event.
    -- , eventModifierFlags                  :: ??            -- ^Modifier keys that were in effect at the time of the event.
    , eventTimestamp                      :: TimeInterval   -- ^Time when the event occured in seconds since system startup.
    , otherEventType                      :: OtherEvent     -- ^The specific type of a tracking-rectangle or cursor-update event.
    -- , otherEventSubtype                   :: Int16
    -- , otherEventData1                     :: Int
    -- , otherEventData2                     :: Int
    }
  deriving (Show)

data KeyEvent
  = KeyDown
  | KeyUp
  | FlagsChanged
  deriving (Eq, Show, Read)

data MouseEvent
  = LeftMouseDown
  | LeftMouseUp
  | LeftMouseDragged
  | RightMouseDown
  | RightMouseUp
  | RightMouseDragged
  | OtherMouseDown
  | OtherMouseUp
  | OtherMouseDragged
  | MouseMoved          -- FIXME: need add support to the scene to allow mouse movement events in the window containing the scene
  deriving (Eq, Show, Read)

data EnterExitEvent
{-
  = MouseEntered
  | MouseExited
-}
  = CursorUpdate  -- ^FIXME: not yet supported
  deriving (Eq, Show, Read)

data OtherEvent
{-
  | AppKitDefined
  | SystemDefined
  | ApplicationDefined
-}
  = Periodic  -- ^FIXME: not yet supported
  deriving (Eq, Show, Read)

-- FIXME: currently not covered:  
{-
  | ScrollWheel
  | TabletPoint
  | TabletProximity
  | EventTypeGesture
  | EventTypeMagnify
  | EventTypeSwipe
  | EventTypeRotate
  | EventTypeBeginGesture
  | EventTypeEndGesture
  | EventTypeSmartMagnify
  | EventTypeQuickLook
-}

-- Marshalling support
-- -------------------

nsEventTypeToKeyEvent :: CLong -> KeyEvent
nsEventTypeToKeyEvent et
  | et == keyDown      = KeyDown
  | et == keyUp        = KeyUp
  | et == flagsChanged = FlagsChanged
  | otherwise                                                        
  = error "Graphics.SpriteKit.Event.nsEventTypeToKeyEvent: out of bounds"

-- NB: Seperate bindings to cache the results
{-# NOINLINE keyDown #-}
keyDown      = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSKeyDown |])
{-# NOINLINE keyUp #-}
keyUp        = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSKeyUp |])
{-# NOINLINE flagsChanged #-}
flagsChanged = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSFlagsChanged |])

keyEvent :: Point -> Double{-TimeInterval-} -> CLong -> String -> String -> Bool -> Word16 -> Event
keyEvent locationInNode timestamp eventType characters charactersIgnoringModifiers isARepeat keyCode
  = KeyEvent
    { eventLocationInNode                 = locationInNode
-- , eventModifierFlags = ??
    , eventTimestamp                      = timestamp
    , keyEventType                        = nsEventTypeToKeyEvent eventType
    , keyEventCharacters                  = characters
    , keyEventCharactersIgnoringModifiers = charactersIgnoringModifiers
    , keyEventIsARepeat                   = isARepeat
    , keyEventKeyCode                     = keyCode
    }

nsEventTypeToMouseEvent :: CLong -> MouseEvent
nsEventTypeToMouseEvent et
  | et == leftMouseDown     = LeftMouseDown
  | et == leftMouseUp       = LeftMouseUp
  | et == leftMouseDragged  = LeftMouseDragged
  | et == rightMouseDown    = RightMouseDown
  | et == rightMouseUp      = RightMouseUp
  | et == rightMouseDragged = RightMouseDragged
  | et == otherMouseDown    = OtherMouseDown
  | et == otherMouseUp      = OtherMouseUp
  | et == otherMouseDragged = OtherMouseDragged
  | et == mouseMoved        = MouseMoved
  | otherwise                                                        
  = error "Graphics.SpriteKit.Event.nsEventTypeToMouseEvent: out of bounds"

-- NB: Seperate bindings to cache the results
{-# NOINLINE leftMouseDown #-}
leftMouseDown     = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSLeftMouseDown |])
{-# NOINLINE leftMouseUp #-}
leftMouseUp       = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSLeftMouseUp |])
{-# NOINLINE leftMouseDragged #-}
leftMouseDragged  = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSLeftMouseDragged |])
{-# NOINLINE rightMouseDown #-}
rightMouseDown    = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSRightMouseDown |])
{-# NOINLINE rightMouseUp #-}
rightMouseUp      = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSRightMouseUp |])
{-# NOINLINE rightMouseDragged #-}
rightMouseDragged = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSRightMouseDragged |])
{-# NOINLINE otherMouseDown #-}
otherMouseDown    = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSOtherMouseDown |])
{-# NOINLINE otherMouseUp #-}
otherMouseUp      = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSOtherMouseUp |])
{-# NOINLINE otherMouseDragged #-}
otherMouseDragged = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSOtherMouseDragged |])
{-# NOINLINE mouseMoved #-}
mouseMoved        = unsafePerformIO $(objc [] $ ''CLong <: [cexp| NSMouseMoved |])

mouseEvent :: Point -> Double{-TimeInterval-} -> CLong -> Int -> Int -> Int -> Float -> Event
mouseEvent locationInNode timestamp eventType number buttonNumber clickCount pressure
  = MouseEvent
    { eventLocationInNode     = locationInNode
-- , eventModifierFlags = ??
    , eventTimestamp          = timestamp
    , mouseEventType          = nsEventTypeToMouseEvent eventType
    , mouseEventNumber        = number
    , mouseEventButtonNumber  = buttonNumber
    , mouseEventClickCount    = clickCount
    , mouseEventPressure      = pressure
    }

objc_emit

event_initialise = objc_initialise
