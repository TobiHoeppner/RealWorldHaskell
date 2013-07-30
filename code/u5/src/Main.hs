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

import KVServer(kvServ)

#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION kvServ
#endif
main = MAIN_FUNCTION

