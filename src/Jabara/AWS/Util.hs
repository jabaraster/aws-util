{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Jabara.AWS.Util (
    AWSProfileName
  , discoverCredentials
  , readCredentials
) where

import           Control.Exception
import           Data.Text
import           Network.AWS
import           System.Directory

discoverCredentials :: AWSProfileName -> IO Credentials
discoverCredentials profileName = do
    catch (readCredentials profileName) $
          \(SomeException e) -> pure $ FromEnv "ENV_AWS_ACCESS_KEY" "ENV_AWS_SECRET_ACCESS_KEY" Nothing Nothing

readCredentials :: AWSProfileName -> IO Credentials
readCredentials profileName = do
    home <- getHomeDirectory
    pure $ FromFile profileName (home ++ "/.aws/credentials")

type AWSProfileName = Text
