{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}


-- this is a backup copy of a test plugin which compiles (but doesn't do anything)

module Ide.Plugin.TestPlugin
where

import Data.Aeson
import Data.Text
import Data.Typeable
import GHC.Generics

import Ide.Types
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Service
import Language.LSP.Types
import Language.LSP.Server


descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pid = (defaultPluginDescriptor pid)
    { pluginHandlers = mkPluginHandler STextDocumentCodeLens codeLensProvider
    , pluginCommands = [ importLensCommand ]
    }


-- 1. request type checking artifacts from the system
-- 2. extract import lists from typechecked ast
-- 3. ask ghc to produce minimal import lists for this ast
-- 4. for every import statement without an explicit import list, find out
--    what the minimal import list is, and produce a code lens to display
--    it together with a diff to graft the import list in
codeLensProvider :: PluginMethodHandler IdeState TextDocumentCodeLens
codeLensProvider state
                 pid
                 CodeLensParams{_textDocument = TextDocumentIdentifier{_uri}}
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri
    = do
        -- typechecking artifacts
        let tmr = runAction "importLens" state $ use TypeCheck nfp
        -- GHC session 
        let hsc = runAction "importLens" state $ use GhcSessionDeps nfp
        -- use ghc api to extract minimal imports
        --(imports, mbMiniImports) <- getMinimalImports hsc tmr
        return $ Right (List [])
    | otherwise = do
        return $ Right (List [])


importCommandId :: CommandId
importCommandId = "ImportLensCommand"


newtype ImportCommandParams = ImportCommandParams WorkspaceEdit
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

--importLensCommand :: PluginCommand a
importLensCommand = 
    PluginCommand importCommandId "Explicit import command" runImportCommand

runImportCommand :: CommandFunction IdeState ImportCommandParams
runImportCommand _ide (ImportCommandParams edit) = do
    return (Right Null)

