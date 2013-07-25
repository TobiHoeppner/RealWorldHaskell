{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Graphics.Gloss

main = display
        (FullScreen (1920, 1080))
        black
        (
            Pictures[
            alive 100 100 10,
            dead 100 120 10
            ]
        )

square = Color black $ Polygon[(-25, 25), (25,25), (25,-25), (-25,-25)]

alive x y z = Translate x y $ Color blue $ Polygon[(-1*z, 1*z), (1*z,1*z), (1*z,-1*z), (-1*z,-1*z)]
dead x y z = Translate x y $ Color white $ Polygon[(-1*z, 1*z), (1*z,1*z), (1*z,-1*z), (-1*z,-1*z)]
