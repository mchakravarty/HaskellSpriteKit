{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards, NamedFieldPuns, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.PhysicsBody
-- Copyright   : [2016] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Description of the physics world that determines the physics simulation for the SpriteKit scene it is associated with.

module Graphics.SpriteKit.PhysicsBody (

  -- ** Representation of the physics properties of a node
  PhysicsBody(..), MassOrDensity(..), ForceImpulse(..),
  
  -- ** Creation of volume-based physics bodies
  bodyWithCircleOfRadius, bodyWithRectangleOfSize, bodyOfBodies, bodyWithPolygonFromPath, bodyWithTextureSize,
  
  -- ** Creation of edge-based physics bodies 
  bodyWithEdgeLoopFromRect, bodyWithEdgeFromPointToPoint, bodyWithEdgeLoopFromPath, bodyWithEdgeChainFromPath,
  
  -- ** Physics body queries
  bodyDensity, bodyArea,

  -- ** Marshalling functions (internal)
  SKPhysicsBody(..),
  physicsBodyToSKPhysicsBody, skPhysicsBodyToPhysicsBody,

  physicsBody_initialise
) where

  -- standard libraries
import Control.Applicative
import Control.Exception as Exc
import Data.Typeable
import Data.Maybe
import Data.Word
import Foreign           hiding (void)
-- import GHC.Prim          (reallyUnsafePtrEquality#)
import System.IO.Unsafe  (unsafePerformIO)
import Unsafe.Coerce     (unsafeCoerce)

  -- friends
import Graphics.SpriteKit.Geometry
import Graphics.SpriteKit.Path
import Graphics.SpriteKit.Texture
import Graphics.SpriteKit.Types

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h", "HaskellSpriteKit/StablePtrBox.h"]


-- FIXME: we need to include this somehow!!!
objc_interface [cunit|

typedef struct CGPath CGPath;

|]
objc_struct_marshaller 'pointToCGPoint 'cgPointToPoint
objc_struct_marshaller 'vectorToCGVector 'cgVectorToVector
objc_struct_marshaller 'sizeToCGSize 'cgSizeToSize
objc_struct_marshaller 'rectToCGRect 'cgRectToRect



-- Creation of volume-based physics bodies
-- ---------------------------------------

-- |Creates a circular physics body.
--
bodyWithCircleOfRadius :: GFloat          -- ^Radius of the circle volume
                       -> Maybe Point     -- ^Optional center (default: centered on owning node’s origin)
                       -> PhysicsBody
bodyWithCircleOfRadius radius Nothing
  = defaultPhysicsBody
      $(objc ['radius :> ''Double {-should be ''GFloat-}] $ Class ''SKPhysicsBody <: 
        [cexp| [SKPhysicsBody bodyWithCircleOfRadius:radius] |])
bodyWithCircleOfRadius radius (Just center)
  = defaultPhysicsBody
      $(objc ['radius :> ''Double {-should be ''GFloat-}, 'center :> ''Point] $ Class ''SKPhysicsBody <: 
        [cexp| [SKPhysicsBody bodyWithCircleOfRadius:radius center:*center] |])

-- |Creates a rectangular physics body.
--
bodyWithRectangleOfSize :: Size           -- ^Size of the rectangle volume
                        -> Maybe Point    -- ^Optional center (default: centered on owning node’s origin)
                        -> PhysicsBody
bodyWithRectangleOfSize size Nothing
  = defaultPhysicsBody
      $(objc ['size :> ''Size] $ Class ''SKPhysicsBody <: 
        [cexp| [SKPhysicsBody bodyWithRectangleOfSize:*size] |])
bodyWithRectangleOfSize size (Just center)
  = defaultPhysicsBody
      $(objc ['size :> ''Size, 'center :> ''Point] $ Class ''SKPhysicsBody <: 
        [cexp| [SKPhysicsBody bodyWithRectangleOfSize:*size center:*center] |])

-- |Creates a physics body by performing a union of a group of volume-based physics bodies.
--
-- NB: The argument list may not contain a body constructed by this very function.
--                      
bodyOfBodies :: [PhysicsBody]             -- ^Component bodies
             -> PhysicsBody
bodyOfBodies bodies
  = defaultPhysicsBody $ do
    { skBodies <- bodiesToSKBodies bodies
    ;  $(objc [ 'skBodies :> Class [t| NSArray SKPhysicsBody |] ] $ Class ''SKPhysicsBody <: 
         [cexp| [SKPhysicsBody bodyWithBodies:skBodies] |])
    }

-- |Creates a polygon-shaped physics body.
--
-- The points are specified relative to the owning node’s origin.
--
bodyWithPolygonFromPath :: Path           -- ^Convex polygonal path with counterclockwise winding & no self intersections
                        -> PhysicsBody
bodyWithPolygonFromPath path
  = defaultPhysicsBody $ do
    { cgPath <- pathToCGPath path
    ; $(objc ['cgPath :> Struct ''CGPath] $ Class ''SKPhysicsBody <: 
        [cexp| [SKPhysicsBody bodyWithPolygonFromPath:cgPath] |])
    }

-- |Creates a physics body from the contents of a texture. 
--
-- The texels in the texture whose alpha values equal or exceed the given alpha threshold are included in the physics
-- body.
--
bodyWithTextureSize :: Texture            -- ^Texture to convert into a physics body
                    -> Maybe GFloat       -- ^Optional alpha threshold (default: 0)
                    -> Size               -- ^Size of the physics body to return
                    -> PhysicsBody
bodyWithTextureSize texture Nothing size
  = let skTexture = textureToSKTexture texture
    in defaultPhysicsBody
        $(objc ['skTexture :> Class ''SKTexture, 'size :> ''Size] $ Class ''SKPhysicsBody <: 
          [cexp| [SKPhysicsBody bodyWithTexture:skTexture size:*size] |])
bodyWithTextureSize texture (Just alphaThreshold) size
  = let skTexture = textureToSKTexture texture
    in defaultPhysicsBody
        $(objc ['skTexture :> Class ''SKTexture, 'alphaThreshold :> ''Float, 'size :> ''Size] $ Class ''SKPhysicsBody <: 
          [cexp| [SKPhysicsBody bodyWithTexture:skTexture alphaThreshold:alphaThreshold size:*size] |])    


-- Creation of edge-based physics bodies
-- -------------------------------------

-- |Creates an edge loop from a rectangle.
--
-- The rectangle is specified relative to the owning node’s origin.
--
bodyWithEdgeLoopFromRect :: Rect          -- ^Rectangle that defines the edges
                         -> PhysicsBody 
bodyWithEdgeLoopFromRect rect
  = defaultPhysicsBody
      $(objc ['rect :> ''Rect] $ Class ''SKPhysicsBody <: 
        [cexp| [SKPhysicsBody bodyWithEdgeLoopFromRect:*rect] |])

-- |Creates an edge between two points.
--
-- The points are specified relative to the owning node’s origin.
--
bodyWithEdgeFromPointToPoint :: Point     -- ^Starting point for the edge
                             -> Point     -- ^Ending point for the edge
                             -> PhysicsBody
bodyWithEdgeFromPointToPoint start end
  = defaultPhysicsBody
      $(objc ['start :> ''Point, 'end :> ''Point] $ Class ''SKPhysicsBody <: 
        [cexp| [SKPhysicsBody bodyWithEdgeFromPoint:*start toPoint:*end] |])

-- Creates an edge loop from a path.
--
-- The path is specified relative to the owning node’s origin. If the path is not already closed, a loop is 
-- automatically created by joining the last point to the first.
--                             
bodyWithEdgeLoopFromPath :: Path          -- ^Path without self intersections
                         -> PhysicsBody
bodyWithEdgeLoopFromPath path
  = defaultPhysicsBody $ do
    { cgPath <- pathToCGPath path
    ; $(objc [ 'cgPath :> ''CGPath ] $ Class ''SKPhysicsBody <: 
        [cexp| [SKPhysicsBody bodyWithEdgeLoopFromPath:cgPath] |])
    }

-- Creates an edge chain from a path.
--
-- The path is specified relative to the owning node’s origin.
--                             
bodyWithEdgeChainFromPath :: Path          -- ^Path without self intersections
                          -> PhysicsBody
bodyWithEdgeChainFromPath path
  = defaultPhysicsBody $ do
    { cgPath <- pathToCGPath path
    ; $(objc [ 'cgPath :> ''CGPath ] $ Class ''SKPhysicsBody <: 
        [cexp| [SKPhysicsBody bodyWithEdgeChainFromPath:cgPath] |])
    }


-- Physics body queries
-- --------------------

-- |Determine a physics body's density. (Marshalling will by default provide the body mass.)
--
bodyDensity :: PhysicsBody -> GFloat
bodyDensity PhysicsBody{bodyForeign}
  = unsafePerformIO $ do
    { skBody <- bodyForeign
    ; $(objc [ 'skBody :> Class ''SKPhysicsBody ] $ ''Double{-should be ''GFloat-} <:
        [cexp| skBody.density |])
    }

-- |Determine the area covered by a physics body.
--
bodyArea :: PhysicsBody -> GFloat
bodyArea PhysicsBody{bodyForeign}
  = unsafePerformIO $ do
    { skBody <- bodyForeign
    ; $(objc [ 'skBody :> Class ''SKPhysicsBody ] $ ''Double{-should be ''GFloat-} <:
        [cexp| skBody.area |])
    }

-- |Determine all nodes whose physics bodies are in contact with the given one.
--
-- allContactedBodies :: PhysicsBody -> [Node u]
-- FIXME: probably only want to return nodes without children; do we need a map function to potentially update all
--        these contacted bodies?

-- TODO: (missing functions)
-- bodyJoints :: PhysicsBody -> [PhysicsJoint]
--


-- Default values
-- --------------

defaultPhysicsBody :: IO SKPhysicsBody -> PhysicsBody
defaultPhysicsBody skPhysicsBody
  = PhysicsBody
    { bodyAffectedByGravity             = True
    , bodyAllowsRotation                = True
    , bodyIsDynamic                     = True
    
    , bodyMassOrDensity                 = Density 1.0
    , bodyFriction                      = 0.2
    , bodyRestitution                   = 0.2
    , bodyLinearDamping                 = 0.1
    , bodyAngularDamping                = 0.1
    , bodyForeign                       = skPhysicsBody
    
    , bodyCategoryBitMask               = 0xFFFFFFFF
    , bodyCollisionBitMask              = 0xFFFFFFFF
    , bodyContactTestBitMask            = 0xFFFFFFFF
    , bodyUsesPreciseCollisionDetection = False
                             
    , bodyForcesAndImpulses             = []
    
    , bodyVelocity                      = vectorZero
    , bodyAngularVelocity               = 0
    , bodyIsResting                     = False
    
    , bodyIsPinned                      = False
    }


-- Marshalling support
-- -------------------

physicsBodyToSKPhysicsBody :: PhysicsBody -> IO SKPhysicsBody
physicsBodyToSKPhysicsBody PhysicsBody{..}
  = do
    { body <- bodyForeign
    ; $(objc [ 'body                              :> Class ''SKPhysicsBody
             , 'bodyAffectedByGravity             :> ''Bool
             , 'bodyAllowsRotation                :> ''Bool
             , 'bodyIsDynamic                     :> ''Bool
                -- FIXME: language-c-inline needs to look through type synonyms
             , 'bodyFriction                      :> ''Double  -- should be ''GFloat
             , 'bodyRestitution                   :> ''Double  -- should be ''GFloat
             , 'bodyLinearDamping                 :> ''Double  -- should be ''GFloat
             , 'bodyAngularDamping                :> ''Double  -- should be ''GFloat
               -- we assume a 64-bit system for the next three (language-c-inline doesn't support 'Word32' right now)
             , 'bodyCategoryBitMask               :> ''CUInt    
             , 'bodyCollisionBitMask              :> ''CUInt
             , 'bodyContactTestBitMask            :> ''CUInt
             , 'bodyUsesPreciseCollisionDetection :> ''Bool
             , 'bodyVelocity                      :> ''Vector
             , 'bodyAngularVelocity               :> ''Double  -- should be ''GFloat
             , 'bodyIsResting                     :> ''Bool
             , 'bodyIsPinned                      :> ''Bool
             ] $ void
        [cexp| ({ 
          body.affectedByGravity             = bodyAffectedByGravity;
          body.allowsRotation                = bodyAllowsRotation;
          body.dynamic                       = bodyIsDynamic;
          body.friction                      = bodyFriction;
          body.restitution                   = bodyRestitution;
          body.linearDamping                 = bodyLinearDamping;
          body.angularDamping                = bodyAngularDamping;
          body.categoryBitMask               = bodyCategoryBitMask;
          body.collisionBitMask              = bodyCollisionBitMask;
          body.contactTestBitMask            = bodyContactTestBitMask;
          body.usesPreciseCollisionDetection = bodyUsesPreciseCollisionDetection;
          body.velocity                      = *bodyVelocity;
          body.angularVelocity               = bodyAngularVelocity;
          body.resting                       = bodyIsResting;
          body.pinned                        = bodyIsPinned;
        }) |])
    ; case bodyMassOrDensity of
        Mass mass       -> $(objc ['body :> Class ''SKPhysicsBody, 'mass :> ''Double{-should be ''GFloat-}] $ void
                             [cexp| body.mass = mass |])
        Density density -> $(objc ['body :> Class ''SKPhysicsBody, 'density :> ''Double{-should be ''GFloat-}] $ void
                             [cexp| body.density = density |])
    ; addForcesAndImpulses body bodyForcesAndImpulses
    ; return body
    }

addForcesAndImpulses body forcesAndImpulses
  = mapM_ add forcesAndImpulses
  where
    add (ApplyForce force Nothing)
      = $(objc ['body :> Class ''SKPhysicsBody, 'force :> ''Vector] $ void 
          [cexp| [body applyForce:*force] |])
    add (ApplyForce force (Just point))
      = $(objc ['body :> Class ''SKPhysicsBody, 'force :> ''Vector, 'point :> ''Point] $ void 
          [cexp| [body applyForce:*force atPoint:*point] |])
    add (ApplyTorque torque)
      = $(objc ['body :> Class ''SKPhysicsBody, 'torque :> ''Double{-should be ''GFloat-}] $ void 
          [cexp| [body applyTorque:torque] |])
    add (ApplyImpulse impulse Nothing)
      = $(objc ['body :> Class ''SKPhysicsBody, 'impulse :> ''Vector] $ void 
          [cexp| [body applyImpulse:*impulse] |])
    add (ApplyImpulse impulse (Just point))
      = $(objc ['body :> Class ''SKPhysicsBody, 'impulse :> ''Vector, 'point :> ''Point] $ void 
          [cexp| [body applyImpulse:*impulse atPoint:*point] |])
    add (ApplyAngularImpulse impulse)
      = $(objc ['body :> Class ''SKPhysicsBody, 'impulse :> ''Double{-should be ''GFloat-}] $ void 
          [cexp| [body applyAngularImpulse:impulse] |])

skPhysicsBodyToPhysicsBody :: SKPhysicsBody -> PhysicsBody
skPhysicsBodyToPhysicsBody skBody
  = PhysicsBody{bodyForeign = return skBody, ..}
  where
    bodyAffectedByGravity  = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Bool <: 
                                 [cexp| skBody.affectedByGravity |])
    bodyAllowsRotation     = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Bool <: 
                                 [cexp| skBody.allowsRotation |])
    bodyIsDynamic          = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Bool <: 
                                 [cexp| skBody.dynamic |])
  
    bodyMassOrDensity      = Mass $ unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Double{-should be ''GFloat-} <: 
                                 [cexp| skBody.mass |])
    bodyFriction           = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Double{-should be ''GFloat-} <: 
                                 [cexp| skBody.friction |])
    bodyRestitution        = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Double{-should be ''GFloat-} <: 
                                 [cexp| skBody.restitution |])
    bodyLinearDamping      = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Double{-should be ''GFloat-} <: 
                                 [cexp| skBody.linearDamping |])
    bodyAngularDamping     = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Double{-should be ''GFloat-} <: 
                                 [cexp| skBody.angularDamping |])
  
    bodyCategoryBitMask    = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''CUInt <: 
                                 [cexp| skBody.categoryBitMask |])
    bodyCollisionBitMask   = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''CUInt <: 
                                 [cexp| skBody.collisionBitMask |])
    bodyContactTestBitMask = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''CUInt <: 
                                 [cexp| skBody.contactTestBitMask |])
    bodyUsesPreciseCollisionDetection 
                           = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Bool <: 
                                 [cexp| skBody.usesPreciseCollisionDetection |])
                           
    bodyForcesAndImpulses  = []
  
    bodyVelocity           = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Vector <: 
                                 [cexp| ({
                                  typename CGVector *vec = (typename CGVector *) malloc(sizeof(CGVector)); 
                                  *vec = skBody.velocity;
                                  vec;
                                 }) |])
    bodyAngularVelocity    = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Double{-should be ''GFloat-} <: 
                                 [cexp| skBody.angularVelocity |])
    bodyIsResting          = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Bool <: 
                                 [cexp| skBody.resting |])
  
    bodyIsPinned           = unsafePerformIO
                               $(objc ['skBody :> Class ''SKPhysicsBody] $  ''Bool <: 
                                 [cexp| skBody.pinned |])

bodiesToSKBodies :: [PhysicsBody] -> IO (NSArray SKPhysicsBody)
bodiesToSKBodies bodies
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray SKPhysicsBody|] <: [cexp| [NSMutableArray arrayWithCapacity:20] |])
    ; mapM_ (addElement marr) bodies
    ; return $ unsafeFreezeNSMutableArray marr
    }
  where
    addElement marr body
      = do
        { skBody <- physicsBodyToSKPhysicsBody body
        ; $(objc ['marr :> Class [t|NSMutableArray SKPhysicsBody|], 'skBody :> Class ''SKPhysicsBody] $ void 
            [cexp| [marr addObject:skBody] |])
        }

objc_emit

physicsBody_initialise = objc_initialise
