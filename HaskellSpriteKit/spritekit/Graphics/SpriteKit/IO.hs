-- |
-- Module      : Graphics.SpriteKit.IO
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
--
-- I/O support for SpriteKit.

module Graphics.SpriteKit.IO (

  lookupFileName

) where

  -- standard libraries
import System.Directory
import System.FilePath
import System.IO


-- |Lookup an asset file name.
--
-- In case of a relative name, the look up behaviour varies between a compiled application bundle and running in the
-- interpreter. In case of a compiled application bundle, the standard SpriteKit bundle behaviour applies. In case of
-- executing in the interpreter, we first look for assets relative to the current working directory.
--
-- FIXME: We need a check for whether we are running in a compiled application.
lookupFileName :: FilePath -> IO FilePath
lookupFileName fname
  -- | <if compiled app> = return fname
  = do
    { cwd <- getCurrentDirectory
    ; let fnameInCurrentDirectory = cwd </> fname
    ; exists <- doesFileExist fnameInCurrentDirectory
    ; return $ if exists then fnameInCurrentDirectory else fname
    }
