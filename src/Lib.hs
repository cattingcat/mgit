
{-# LANGUAGE OverloadedStrings #-}

module Lib (
  
) where

{-
import Git
import Git.Libgit2 hiding (getRepository)
import Control.Monad.IO.Class
import Data.Tagged
import Data.Time
import qualified Data.Text as T


someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"
  gitTest

gitTest :: IO ()
gitTest = do
    let repoOpts = RepositoryOptions { repoPath = "."
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }

    withRepository' lgFactory repoOpts $ do
      f <- facts
      liftIO $ print f

      rep <- getRepository
      liftIO $ print (lgRepoPath rep)

      maybeHead <- resolveReference "HEAD"
      liftIO $ print maybeHead
      
      case maybeHead of 
        Just id -> do
          commit <- lookupCommit (Tagged id)
          liftIO $ print (commitLog commit)
          
          tag <- lookupTag (Tagged id)
          liftIO $ print (tagOid tag)


      pure()
    pure ()-}
