{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, FlexibleInstances, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.PhysicsWorld
-- Copyright   : [2016] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Description of the physics world that determines the physics simulation for the SpriteKit scene it is associated with.

module Graphics.SpriteKit.PhysicsWorld (

  -- ** Representation of a physics simulation for one scene
  PhysicsWorld(..), PhysicsContact(..), PhysicsContactHandler, physicsWorld,

  -- ** Marshalling functions (internal)
  SKPhysicsWorld(..), SKPhysicsContact(..), skPhysicsContactToPhysicsContact,

  physicsWorld_initialise
) where

  -- standard libraries
import Control.Applicative
import Control.Exception as Exc
import Data.Typeable
import Data.Maybe
import Foreign           hiding (void)
-- import GHC.Prim          (reallyUnsafePtrEquality#)
import System.IO.Unsafe  (unsafePerformIO)
import Unsafe.Coerce     (unsafeCoerce)

  -- friends
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Node
import Graphics.SpriteKit.Types

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h", "HaskellSpriteKit/StablePtrBox.h"]


-- Physics world
-- -------------

-- |SpriteKit physics world.
--
data PhysicsWorld sceneData nodeData 
  = PhysicsWorld
    { worldGravity         :: Vector -- ^Gravitational acceleration applied to bodies in the physics world (default: '(0.0,-9.8)')
    , worldSpeed           :: GFloat -- ^Simulation rate as multiple of scene simulation rate (default: '1.0')
    , worldContactDidBegin :: Maybe (PhysicsContactHandler sceneData nodeData)
    , worldContactDidEnd   :: Maybe (PhysicsContactHandler sceneData nodeData)
    }
    -- TODO: (missing fields)
    -- * list of directives for joint manipulation ('addJoint;, 'removeAllJoints', 'removeJoint')

-- |Information that chracterises the nature of a given contact between two physics bodies.
--
data PhysicsContact nodeData
  = PhysicsContact
    { contactBodyA            :: Node nodeData -- ^First body in the contact
    , contactBodyB            :: Node nodeData -- ^Second body in the contact
    , contactPoint            :: Point         -- ^Contact point between the two associated physics bodies, in scene coordinates
    , contactCollisionImpulse :: GFloat        -- ^Impulse that specifies how hard these two bodies struck each other in newton-seconds
    , contactNormal           :: Vector        -- ^Normal vector specifying the direction of the collision
    }
    
-- |The function invoked when contact between two physics bodies either begins or ends.
--
-- This handler function gets access to the current scene user data as well as the two nodes associated with the 
-- contacting physics bodies. It can modify the scene user data and/or these two nodes to affect any changes that
-- need to be made to the scene due to the beginning or ending of the contact. Only modified data needs to be returned.
--
type PhysicsContactHandler sceneData nodeData 
  =  sceneData                                                          -- ^Current scene data
  -> PhysicsContact nodeData                                            -- ^Contact information
  -> (Maybe sceneData, Maybe (Node nodeData), Maybe (Node nodeData))


-- Create a physics world
-- ----------------------

-- The default physics world.
--
physicsWorld :: PhysicsWorld sceneData nodeData
physicsWorld
  = PhysicsWorld
    { worldGravity         = Vector 0.0 (-9.8)
    , worldSpeed           = 1
    , worldContactDidBegin = Nothing
    , worldContactDidEnd   = Nothing
    }


-- Physics world queries
-- ---------------------

-- FIXME: Features not yet supported:
-- * searching for bodies: 'bodyAlongRayStart:end:', 'bodyAtPoint:', 'bodyInRect:', 
--   'enumerateBodiesAlongRayStart:end:usingBlock:', 'enumerateBodiesAtPoint:usingBlock:', 'enumerateBodiesInRect:usingBlock:',
-- * sampling fields: 'sampleFieldsAt:'


-- Marshalling support
-- -------------------

objc_marshaller 'pointToCGPoint   'cgPointToPoint
objc_marshaller 'vectorToCGVector 'cgVectorToVector

newtype SKPhysicsWorld = SKPhysicsWorld (ForeignPtr SKPhysicsWorld)
  deriving Typeable   -- needed for now until migrating to new TH

newtype SKPhysicsContact = SKPhysicsContact (ForeignPtr SKPhysicsContact)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

skPhysicsContactToPhysicsContact :: SKPhysicsContact -> PhysicsContact nodeData
skPhysicsContactToPhysicsContact skContact
  = PhysicsContact{..}      
  where
    contactBodyA            = unsafePerformIO $ do
                              { skNode <- $(objc [ 'skContact :> Class ''SKPhysicsContact ] $ Class ''SKNode <: 
                                            [cexp| skContact.bodyA.node |])  -- FIXME: in principle, this can be nil,
                                                                             --  but can it be in a contact??
                              ; skNodeToNode skNode
                              }
    contactBodyB            = unsafePerformIO $ do
                              { skNode <- $(objc [ 'skContact :> Class ''SKPhysicsContact ] $ Class ''SKNode <: 
                                            [cexp| skContact.bodyB.node |])  -- FIXME: in principle, this can be nil,
                                                                             --  but can it be in a contact??
                              ; skNodeToNode skNode
                              }
    contactPoint            = unsafePerformIO $(objc [ 'skContact :> Class ''SKPhysicsContact ] $ ''Point <: 
                                                [cexp| ({
                                                  typename CGPoint *pnt = (typename CGPoint *) malloc(sizeof(CGPoint)); 
                                                  *pnt = ((typename SKPhysicsContact*)skContact).contactPoint;
                                                  pnt;
                                                 }) |])
    contactCollisionImpulse = unsafePerformIO $(objc [ 'skContact :> Class ''SKPhysicsContact ] $ ''Double{-GFloat-} <: 
                                                [cexp| skContact.collisionImpulse |])
    contactNormal           = unsafePerformIO $(objc [ 'skContact :> Class ''SKPhysicsContact ] $ ''Vector <: 
                                                [cexp| ({
                                                  typename CGVector *vec = (typename CGVector *) malloc(sizeof(CGVector)); 
                                                  *vec = ((typename SKPhysicsContact*)skContact).contactNormal;
                                                  vec;
                                                 }) |])

objc_emit

physicsWorld_initialise = objc_initialise
