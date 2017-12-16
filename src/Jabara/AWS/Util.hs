{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Jabara.AWS.Util (
    AWSProfileName

  , discoverCredentials
  , discoverEnv
  , readCredentials

  , localDynamoDBEnv
) where

import           Control.Exception
import           Control.Lens
import           Data.Monoid
import           Data.Text
import           Network.AWS
import           Network.AWS.Auth
import           System.Directory

type AWSProfileName = Text

discoverEnv :: AWSProfileName -> Region -> IO Env
discoverEnv profileName region = do
    env <- newEnv =<< discoverCredentials profileName
    pure $ env&envRegion .~ region

discoverCredentials :: AWSProfileName -> IO Credentials
discoverCredentials profileName = do
    catch (readCredentials profileName) $
          \(SomeException e) -> pure $ FromEnv "ENV_AWS_ACCESS_KEY" "ENV_AWS_SECRET_ACCESS_KEY" Nothing Nothing

readCredentials :: AWSProfileName -> IO Credentials
readCredentials profileName = do
    home <- getHomeDirectory
    pure $ FromFile profileName (home ++ "/.aws/credentials")

localDynamoDBEnv :: IO Env
localDynamoDBEnv = do
    env <- newEnv $ FromKeys "" ""
    pure $ env&envOverride .~ localOverride
  where
    localOverride :: Dual (Endo Service)
    localOverride = Dual $ Endo localService

    localService :: Service -> Service
    localService svc = svc { _svcEndpoint = \_ -> localEndpoint }

    localEndpoint :: Endpoint
    localEndpoint = Endpoint {
                      _endpointHost = "localhost"
                    , _endpointSecure = False
                    , _endpointPort = 8000
                    , _endpointScope = ""
                    }
