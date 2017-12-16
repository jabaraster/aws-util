{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Jabara.AWS.Util (
    AWSProfile(..)
  , AWSProfileName
  , apAccessKey
  , apSecretAccessKey

  , readLocalAWSProfile
  , readLocalAWSProfile'
) where

import           Control.Lens
import           Control.Lens.TH
import           Data.Ini
import           Data.Text
import           System.Directory

readLocalAWSProfile :: AWSProfileName -> IO (Either String AWSProfile)
readLocalAWSProfile profileName = do
    ini <- getHomeDirectory
             >>= \h -> readIniFile (h ++ "/.aws/credentials")
             >>= \i ->
                     case i of
                         Left s    -> error s
                         Right ini -> pure ini
    pure $ core profileName ini

readLocalAWSProfile' :: FilePath -> AWSProfileName -> IO (Either String AWSProfile)
readLocalAWSProfile' credentialsFilePath profileName = do
    ini <- readIniFile credentialsFilePath
             >>= \i ->
                     case i of
                         Left s    -> error s
                         Right ini -> pure ini
    pure $ core profileName ini

core :: AWSProfileName -> Ini -> Either String AWSProfile
core profileName ini =
    let key = lookupValue profileName "aws_access_key_id" ini
        sec = lookupValue profileName "aws_secret_access_key" ini
    in case (key, sec) of
        (   Left ks,   Left ss ) -> Left (ks ++ "/" ++ ss)
        (l@(Left s),   _       ) -> Left s
        (   _      , l@(Left s)) -> Left s
        (  Right k ,   Right s ) -> Right $ AWSProfile {_apAccessKey = k, _apSecretAccessKey = s}

type AWSProfileName = Text

data AWSProfile
    = AWSProfile
      { _apAccessKey       :: Text
      , _apSecretAccessKey :: Text
      }
    deriving (Show, Read, Eq)
makeLenses ''AWSProfile
