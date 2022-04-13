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
    { pluginHandlers = mkPluginHandler STextDocumentCodeLens lensProvider
    }


--type PluginMethodHandler a m = a -> PluginId -> MessageParams m -> LspM Config (Either ResponseError (ResponseResult m))

lensProvider :: PluginMethodHandler IdeState TextDocumentCodeLens
lensProvider state pid config = do
    return $ Right (List [helloCodeLens])


helloCodeLens :: CodeLens
helloCodeLens = CodeLens { _range   = Range (Position 0 0) (Position 1 0)
                         , _command = Just $ Command "hello codelens!"
                                                      "id"
                                                      Nothing
                         , _xdata   = Nothing
                         }
