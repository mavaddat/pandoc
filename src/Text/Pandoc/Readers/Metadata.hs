{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Readers.Metadata
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parse YAML/JSON metadata to 'Pandoc' 'Meta'.
-}
module Text.Pandoc.Readers.Metadata (
  yamlBsToMeta,
  yamlBsToRefs,
  yamlMetaBlock,
  yamlMap ) where


import Control.Monad.Except (throwError)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Internal as Yaml
import qualified Text.Libyaml as Y
import Data.Aeson (Value(..), Object, Result(..), fromJSON, (.:?), withObject,
                   FromJSON)
import Data.Aeson.Types (formatRelativePath, parse)
import Text.Pandoc.Shared (tshow, blocksToInlines)
import Text.Pandoc.Class (PandocMonad (..), report)
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging (LogMessage(YamlWarning))
import Text.Pandoc.Parsing hiding (tableWith, parse)
import qualified Text.Pandoc.UTF8 as UTF8
import System.IO.Unsafe (unsafePerformIO)

yamlBsToMeta :: (PandocMonad m, HasLastStrPosition st)
             => ParsecT Sources st m (Future st MetaValue)
             -> B.ByteString
             -> ParsecT Sources st m (Future st Meta)
yamlBsToMeta pMetaValue bstr = do
  pos <- getPosition
  case decodeAllWithWarnings bstr of
       Right (warnings, xs) -> do
         mapM_ (\(Yaml.DuplicateKey jpath) ->
                          report (YamlWarning pos $ "Duplicate key: " <>
                                  T.pack (formatRelativePath jpath)))
           warnings
         case xs of
           (Object o : _) -> fmap Meta <$> yamlMap pMetaValue o
           [Null] -> return . return $ mempty
           [] -> return . return $ mempty
           _  -> Prelude.fail "expected YAML object"
       Left err' -> do
         let msg = T.pack $ "Error parsing YAML metadata at " <>
                             show pos <> ":\n" <>
                             Yaml.prettyPrintParseException err'
         throwError $ PandocParseError $
           if "did not find expected key" `T.isInfixOf` msg
              then msg <>
                   "\nConsider enclosing the entire field in 'single quotes'"
              else msg

decodeAllWithWarnings :: FromJSON a
                      => B.ByteString
                      -> (Either Yaml.ParseException ([Yaml.Warning], [a]))
decodeAllWithWarnings = either Left (\(ws,res)
                       -> case res of
                            Left s  -> Left (Yaml.AesonException s)
                            Right v -> Right (ws, v))
                   . unsafePerformIO
                   . Yaml.decodeAllHelper
                   . Y.decode

-- Returns filtered list of references.
yamlBsToRefs :: (PandocMonad m, HasLastStrPosition st)
             => ParsecT Sources st m (Future st MetaValue)
             -> (Text -> Bool) -- ^ Filter for id
             -> B.ByteString
             -> ParsecT Sources st m (Future st [MetaValue])
yamlBsToRefs pMetaValue idpred bstr =
  case Yaml.decodeAllEither' bstr of
       Right (Object m : _) -> do
         case parse (withObject "metadata" (.:? "references")) (Object m) of
           Success (Just refs) -> sequence <$>
                 mapM (yamlToMetaValue pMetaValue) (filter hasSelectedId refs)
           _ -> return $ return []
       Right (Array v : _) -> do
         let refs = filter hasSelectedId $ V.toList v
         sequence <$> mapM (yamlToMetaValue pMetaValue) (filter hasSelectedId refs)
       Right _ -> return . return $ []
       Left err' -> throwError $ PandocParseError
                               $ T.pack $ Yaml.prettyPrintParseException err'
 where
   isSelected (String t) = idpred t
   isSelected _ = False
   hasSelectedId (Object o) =
     case parse (withObject "ref" (.:? "id")) (Object o) of
       Success (Just id') -> isSelected id'
       _ -> False
   hasSelectedId _ = False

normalizeMetaValue :: (PandocMonad m, HasLastStrPosition st)
                   => ParsecT Sources st m (Future st MetaValue)
                   -> Text
                   -> ParsecT Sources st m (Future st MetaValue)
normalizeMetaValue pMetaValue x =
   -- Note: a standard quoted or unquoted YAML value will
   -- not end in a newline, but a "block" set off with
   -- `|` or `>` will.
   if "\n" `T.isSuffixOf` (T.dropWhileEnd isSpaceChar x) -- see #6823
      then parseFromString' pMetaValue (x <> "\n\n")
      else try (parseFromString' asInlines x') -- see #8358
           <|> -- see #8465
           parseFromString' asInlines (x' <> "\n\n")
  where x' = T.dropWhile isSpaceOrNlChar x
        asInlines = fmap b2i <$> pMetaValue
        b2i (MetaBlocks bs) = MetaInlines (blocksToInlines bs)
        b2i y = y
        isSpaceChar ' '  = True
        isSpaceChar '\t' = True
        isSpaceChar _    = False
        isSpaceOrNlChar '\r' = True
        isSpaceOrNlChar '\n' = True
        isSpaceOrNlChar c = isSpaceChar c

yamlToMetaValue :: (PandocMonad m, HasLastStrPosition st)
                => ParsecT Sources st m (Future st MetaValue)
                -> Value
                -> ParsecT Sources st m (Future st MetaValue)
yamlToMetaValue pMetaValue v =
  case v of
       String t -> normalizeMetaValue pMetaValue t
       Bool b -> return $ return $ MetaBool b
       Number d -> normalizeMetaValue pMetaValue $
         case fromJSON v of
           Success (x :: Int) -> tshow x
           _ -> tshow d
       Null -> return $ return $ MetaString ""
       Array{} -> do
         case fromJSON v of
           Error err' -> throwError $ PandocParseError $ T.pack err'
           Success xs -> fmap MetaList . sequence <$>
                          mapM (yamlToMetaValue pMetaValue) xs
       Object o -> fmap MetaMap <$> yamlMap pMetaValue o

yamlMap :: (PandocMonad m, HasLastStrPosition st)
        => ParsecT Sources st m (Future st MetaValue)
        -> Object
        -> ParsecT Sources st m (Future st (M.Map Text MetaValue))
yamlMap pMetaValue o = do
    case fromJSON (Object o) of
      Error err' -> throwError $ PandocParseError $ T.pack err'
      Success (m' :: M.Map Text Value) -> do
        let kvs = filter (not . ignorable . fst) $ M.toList m'
        fmap M.fromList . sequence <$> mapM toMeta kvs
  where
    ignorable t = "_" `T.isSuffixOf` t
    toMeta (k, v) = do
      fv <- yamlToMetaValue pMetaValue v
      return $ do
        v' <- fv
        return (k, v')

-- | Parse a YAML metadata block using the supplied 'MetaValue' parser.
yamlMetaBlock :: (HasLastStrPosition st, PandocMonad m)
              => ParsecT Sources st m (Future st MetaValue)
              -> ParsecT Sources st m (Future st Meta)
yamlMetaBlock parser = try $ do
  pos <- getPosition
  string "---"
  blankline
  notFollowedBy blankline  -- if --- is followed by a blank it's an HRULE
  rawYamlLines <- manyTill anyLine stopLine
  -- by including --- and ..., we allow yaml blocks with just comments:
  let rawYaml = T.unlines ("---" : (rawYamlLines ++ ["..."]))
  optional blanklines
  oldPos <- getPosition
  setPosition pos
  res <- yamlBsToMeta parser $ UTF8.fromText rawYaml
  setPosition oldPos
  pure res

stopLine :: Monad m => ParsecT Sources st m ()
stopLine = try $ (string "---" <|> string "...") >> blankline >> return ()
