{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_minilisp (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "minilisp"
version :: Version
version = Version [1,0,0] []

synopsis :: String
synopsis = "MiniLisp interpreter in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/usuario/minilisp"
