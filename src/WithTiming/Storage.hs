module WithTiming.Storage (getPreviousResult, updateResultFile) where

import           Control.Exception        (SomeException, try)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.Aeson               as JSON
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import qualified Data.Map                 as Map

type ResultMap = Map.Map String Integer

parseResultMap :: B.ByteString -> Maybe ResultMap
parseResultMap = JSON.decode

rightOrEmpty :: Monoid b => Either a b -> b
rightOrEmpty e = case e of
                   Left _  -> mempty
                   Right b -> b

readFileSafely :: MonadIO io => FilePath -> io (Either SomeException B.ByteString)
readFileSafely path = liftIO $ try (B.readFile path)

readResultFile :: MonadIO io => FilePath -> io (Either String ResultMap)
readResultFile path = do
  contents' <- liftIO $ readFileSafely path
  case contents' of
    Left ex -> return $ Left $ show ex
    Right contents -> case parseResultMap contents of
                        Nothing -> return $ Left $ "Unable to parse " ++ (show path) ++ " as an object from String -> Integer."
                        Just map -> return $ Right map

-- this function reads the file again, even though we've definitely already read it once.
-- we could just use the old data and save ourselves some risky IO, but that would make the whole thing much racier.
-- I'd like to avoid a scenario where one instance of this has its results clobbered by another.
-- maybe that's a good argument for using a directory of files instead of a file with JSON keys.
updateResultFile :: MonadIO io => FilePath -> String -> Integer -> io ()
updateResultFile path key duration = do
  oldMap' <- readResultFile path
  let oldMap = rightOrEmpty oldMap'
  let newMap = Map.insert key duration oldMap
  liftIO $ B.writeFile path (encodePretty newMap)

getPreviousResult :: MonadIO io => FilePath -> String -> io (Maybe Integer)
getPreviousResult path key = do
  contents' <- readResultFile path
  case contents' of
    Left err  -> return Nothing
    Right map -> return $ Map.lookup key map
