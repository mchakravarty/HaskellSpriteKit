{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ForeignFunctionInterface #-}

-- |
-- Module      : Graphics.SpriteKit.Texture
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- Textures

module Graphics.SpriteKit.Texture (
  Texture,
  textureWithImageNamed, textureSize,

  -- * Marshalling support
  SKTexture(..),
  
  texture_initialise
) where

  -- standard libraries
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

  -- friends
import Graphics.SpriteKit.Geometry

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "<SpriteKit/SpriteKit.h>", "GHC/HsFFI.h"]

objc_marshaller 'sizeToCGSize 'cgSizeToSize


-- |A SpriteKit texture object
--
type Texture = SKTexture

-- 'SKTexture' objects are immutable; so, we represent them in Haskell as a foreign pointer to the
-- native representation.
--
newtype SKTexture = SKTexture (ForeignPtr SKTexture)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck

textureWithImageNamed :: FilePath -> Texture
textureWithImageNamed fname
  = unsafePerformIO $(objc ['fname :> ''String] $ Class ''SKTexture <: [cexp| [SKTexture textureWithImageNamed:fname] |])

textureSize :: Texture -> Size
textureSize texture
  = unsafePerformIO $(objc ['texture :> Class ''SKTexture] $ ''Size <:
                      [cexp| ({ 
                        typename CGSize *sz = (typename CGSize *) malloc(sizeof(CGSize)); 
                        *sz = [texture size]; 
                        sz; 
                      }) |] )

-- objc_interface [cunit|
--   void Texture_initialise(void);
-- |]

objc_emit

texture_initialise = objc_initialise

-- foreign export ccall "Texture_initialise" objc_initialise :: IO ()
