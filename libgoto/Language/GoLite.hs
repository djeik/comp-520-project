{-|
Module      : Language.GoLite
Description : Core GoLite module
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module merely reexports the most important submodules for dealing with
GoLite code.
-}

{-# LANGUAGE OverloadedStrings #-}

module Language.GoLite
( module Language.GoLite.Syntax.Types
, module Language.GoLite.Parser
, module Language.GoLite.Lexer
) where

import Language.GoLite.Syntax.Types
import Language.GoLite.Parser
import Language.GoLite.Lexer
