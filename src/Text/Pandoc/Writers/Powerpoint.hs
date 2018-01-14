{-# LANGUAGE PatternGuards, MultiWayIf, OverloadedStrings #-}

{-
Copyright (C) 2017-2018 Jesse Rosenthal <jrosenthal@jhu.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Powerpoint
   Copyright   : Copyright (C) 2017-2018 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to powerpoint (pptx).
-}

module Text.Pandoc.Writers.Powerpoint (writePowerpoint) where

import Control.Monad.Except (throwError, catchError)
import Control.Monad.Reader
import Control.Monad.State
import Codec.Archive.Zip
import Data.List (intercalate, stripPrefix, nub, union)
import Data.Default
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import System.FilePath.Posix (splitDirectories, splitExtension, takeExtension)
import Text.XML.Light
import qualified Text.XML.Light.Cursor as XMLC
import Text.Pandoc.Definition
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Slides (getSlideLevel)
import qualified Text.Pandoc.Class as P
import Text.Pandoc.Options
import Text.Pandoc.MIME
import Text.Pandoc.Logging
import qualified Data.ByteString.Lazy as BL
import Text.Pandoc.Walk
import qualified Text.Pandoc.Shared as Shared -- so we don't overlap "Element"
import Text.Pandoc.Writers.Shared (fixDisplayMath, metaValueToInlines)
import Text.Pandoc.Writers.OOXML
import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe, maybeToList, catMaybes)
import Text.Pandoc.ImageSize
import Control.Applicative ((<|>))
import System.FilePath.Glob

import Text.TeXMath
import Text.Pandoc.Writers.Math (convertMath)


writePowerpoint :: (PandocMonad m)
                => WriterOptions  -- ^ Writer options
                -> Pandoc         -- ^ Document to convert
                -> m BL.ByteString
writePowerpoint opts (Pandoc meta blks) = do
  let blks' = walk fixDisplayMath blks
  distArchive <- (toArchive . BL.fromStrict) <$>
                      P.readDefaultDataFile "reference.pptx"
  refArchive <- case writerReferenceDoc opts of
                     Just f  -> toArchive <$> P.readFileLazy f
                     Nothing -> (toArchive . BL.fromStrict) <$>
                        P.readDataFile "reference.pptx"

  utctime <- P.getCurrentTime

  presSize <- case getPresentationSize refArchive distArchive of
                Just sz -> return sz
                Nothing -> throwError $
                           PandocSomeError $
                           "Could not determine presentation size"

  let env = def { envMetadata = meta
                , envRefArchive = refArchive
                , envDistArchive = distArchive
                , envUTCTime = utctime
                , envOpts = opts
                , envSlideLevel = case writerSlideLevel opts of
                                    Just n -> n
                                    Nothing -> getSlideLevel blks'
                , envPresentationSize = presSize
                }

  let st = def { stMediaGlobalIds = initialGlobalIds refArchive distArchive
               }

  runP env st $ do pres <- blocksToPresentation blks'
                   archv <- presentationToArchive pres
                   return $ fromArchive archv

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

data WriterEnv = WriterEnv { envMetadata :: Meta
                           , envRunProps :: RunProps
                           , envParaProps :: ParaProps
                           , envSlideLevel :: Int
                           , envRefArchive :: Archive
                           , envDistArchive :: Archive
                           , envUTCTime :: UTCTime
                           , envOpts :: WriterOptions
                           , envPresentationSize :: (Integer, Integer)
                           , envSlideHasHeader :: Bool
                           , envInList :: Bool
                           , envInNoteSlide :: Bool
                           , envCurSlideId :: Int
                           -- the difference between the number at
                           -- the end of the slide file name and
                           -- the rId number
                           , envSlideIdOffset :: Int
                           , envColumnNumber :: Maybe Int
                           }
                 deriving (Show)

instance Default WriterEnv where
  def = WriterEnv { envMetadata = mempty
                  , envRunProps = def
                  , envParaProps = def
                  , envSlideLevel = 2
                  , envRefArchive = emptyArchive
                  , envDistArchive = emptyArchive
                  , envUTCTime = posixSecondsToUTCTime 0
                  , envOpts = def
                  , envPresentationSize = (720, 540)
                  , envSlideHasHeader = False
                  , envInList = False
                  , envInNoteSlide = False
                  , envCurSlideId = 1
                  , envSlideIdOffset = 1
                  , envColumnNumber = Nothing
                  }

data MediaInfo = MediaInfo { mInfoFilePath :: FilePath
                           , mInfoLocalId  :: Int
                           , mInfoGlobalId :: Int
                           , mInfoMimeType :: Maybe MimeType
                           , mInfoExt      :: Maybe String
                           , mInfoCaption  :: Bool
                           } deriving (Show, Eq)

data WriterState = WriterState { stLinkIds :: M.Map Int (M.Map Int (URL, String))
                               -- (FP, Local ID, Global ID, Maybe Mime)
                               , stMediaIds :: M.Map Int [MediaInfo]
                               , stMediaGlobalIds :: M.Map FilePath Int
                               , stNoteIds :: M.Map Int [Block]
                               -- associate anchors with slide id
                               , stAnchorMap :: M.Map String Int
                               -- media inherited from the template.
                               , stTemplateMedia :: [FilePath]
                               } deriving (Show, Eq)

instance Default WriterState where
  def = WriterState { stLinkIds = mempty
                    , stMediaIds = mempty
                    , stMediaGlobalIds = mempty
                    , stNoteIds = mempty
                    , stAnchorMap= mempty
                    , stTemplateMedia = []
                    }

-- This populates the global ids map with images already in the
-- template, so the ids won't be used by images introduced by the
-- user.
initialGlobalIds :: Archive -> Archive -> M.Map FilePath Int
initialGlobalIds refArchive distArchive =
  let archiveFiles = filesInArchive refArchive `union` filesInArchive distArchive
      mediaPaths = filter (match (compile "ppt/media/image")) archiveFiles

      go :: FilePath -> Maybe (FilePath, Int)
      go fp = do
        s <- stripPrefix "ppt/media/image" $ fst $ splitExtension fp
        (n, _) <- listToMaybe $ reads s
        return (fp, n)
  in
    M.fromList $ mapMaybe go mediaPaths

getPresentationSize :: Archive -> Archive -> Maybe (Integer, Integer)
getPresentationSize refArchive distArchive = do
  entry <- findEntryByPath "ppt/presentation.xml" refArchive  `mplus`
           findEntryByPath "ppt/presentation.xml" distArchive
  presElement <- parseXMLDoc $ UTF8.toStringLazy $ fromEntry entry
  let ns = elemToNameSpaces presElement
  sldSize <- findChild (elemName ns "p" "sldSz") presElement
  cxS <- findAttr (QName "cx" Nothing Nothing) sldSize
  cyS <- findAttr (QName "cy" Nothing Nothing) sldSize
  (cx, _) <- listToMaybe $ reads cxS :: Maybe (Integer, String)
  (cy, _) <- listToMaybe $ reads cyS :: Maybe (Integer, String)
  return (cx `div` 12700, cy `div` 12700)

type P m = ReaderT WriterEnv (StateT WriterState m)

runP :: Monad m => WriterEnv -> WriterState -> P m a -> m a
runP env st p = evalStateT (runReaderT p env) st

type Pixels = Integer

data Presentation = Presentation [Slide]
  deriving (Show)

data Slide = MetadataSlide { metadataSlideTitle :: [ParaElem]
                            , metadataSlideSubtitle :: [ParaElem]
                            , metadataSlideAuthors :: [[ParaElem]]
                            , metadataSlideDate :: [ParaElem]
                            }
           | TitleSlide { titleSlideHeader :: [ParaElem]}
           | ContentSlide { contentSlideHeader :: [ParaElem]
                          , contentSlideContent :: [Shape]
                          }
           | TwoColumnSlide { twoColumnSlideHeader :: [ParaElem]
                            , twoColumnSlideLeft   :: [Shape]
                            , twoColumnSlideRight  :: [Shape]
                            }
           deriving (Show, Eq)

data SlideElement = SlideElement Pixels Pixels Pixels Pixels Shape
  deriving (Show, Eq)

data Shape = Pic PicProps FilePath Text.Pandoc.Definition.Attr [ParaElem]
           | GraphicFrame [Graphic] [ParaElem]
           | TextBox [Paragraph]
  deriving (Show, Eq)

type Cell = [Paragraph]

data TableProps = TableProps { tblPrFirstRow :: Bool
                             , tblPrBandRow :: Bool
                             } deriving (Show, Eq)

type ColWidth = Integer

data Graphic = Tbl TableProps [ColWidth] [Cell] [[Cell]]
  deriving (Show, Eq)


data Paragraph = Paragraph { paraProps :: ParaProps
                           , paraElems  :: [ParaElem]
                           } deriving (Show, Eq)

autoNumberingToType :: ListAttributes -> String
autoNumberingToType (_, numStyle, numDelim) =
  typeString ++ delimString
  where
    typeString = case numStyle of
      Decimal -> "arabic"
      UpperAlpha -> "alphaUc"
      LowerAlpha -> "alphaLc"
      UpperRoman -> "romanUc"
      LowerRoman -> "romanLc"
      _          -> "arabic"
    delimString = case numDelim of
      Period -> "Period"
      OneParen -> "ParenR"
      TwoParens -> "ParenBoth"
      _         -> "Period"

data BulletType = Bullet
                | AutoNumbering ListAttributes
  deriving (Show, Eq)

data Algnment = AlgnLeft | AlgnRight | AlgnCenter
  deriving (Show, Eq)

data ParaProps = ParaProps { pPropMarginLeft :: Maybe Pixels
                           , pPropMarginRight :: Maybe Pixels
                           , pPropLevel :: Int
                           , pPropBullet :: Maybe BulletType
                           , pPropAlign :: Maybe Algnment
                           , pPropSpaceBefore :: Maybe Pixels
                           } deriving (Show, Eq)

instance Default ParaProps where
  def = ParaProps { pPropMarginLeft = Just 0
                  , pPropMarginRight = Just 0
                  , pPropLevel = 0
                  , pPropBullet = Nothing
                  , pPropAlign = Nothing
                  , pPropSpaceBefore = Nothing
                  }

newtype TeXString = TeXString {unTeXString :: String}
  deriving (Eq, Show)

data ParaElem = Break
              | Run RunProps String
              -- It would be more elegant to have native TeXMath
              -- Expressions here, but this allows us to use
              -- `convertmath` from T.P.Writers.Math. Will perhaps
              -- revisit in the future.
              | MathElem MathType TeXString
              deriving (Show, Eq)

data Strikethrough = NoStrike | SingleStrike | DoubleStrike
  deriving (Show, Eq)

data Capitals = NoCapitals | SmallCapitals | AllCapitals
  deriving (Show, Eq)

type URL = String

data RunProps = RunProps { rPropBold :: Bool
                         , rPropItalics :: Bool
                         , rStrikethrough :: Maybe Strikethrough
                         , rBaseline :: Maybe Int
                         , rCap :: Maybe Capitals
                         , rLink :: Maybe (URL, String)
                         , rPropCode :: Bool
                         , rPropBlockQuote :: Bool
                         , rPropForceSize :: Maybe Pixels
                         } deriving (Show, Eq)

instance Default RunProps where
  def = RunProps { rPropBold = False
                 , rPropItalics = False
                 , rStrikethrough = Nothing
                 , rBaseline = Nothing
                 , rCap = Nothing
                 , rLink = Nothing
                 , rPropCode = False
                 , rPropBlockQuote = False
                 , rPropForceSize = Nothing
                 }

data PicProps = PicProps { picPropLink :: Maybe (URL, String)
                         } deriving (Show, Eq)

instance Default PicProps where
  def = PicProps { picPropLink = Nothing
                 }

--------------------------------------------------

inlinesToParElems :: Monad m => [Inline] -> P m [ParaElem]
inlinesToParElems ils = concatMapM inlineToParElems ils

inlineToParElems :: Monad m => Inline -> P m [ParaElem]
inlineToParElems (Str s) = do
  pr <- asks envRunProps
  return [Run pr s]
inlineToParElems (Emph ils) =
  local (\r -> r{envRunProps = (envRunProps r){rPropItalics=True}}) $
  inlinesToParElems ils
inlineToParElems (Strong ils) =
  local (\r -> r{envRunProps = (envRunProps r){rPropBold=True}}) $
  inlinesToParElems ils
inlineToParElems (Strikeout ils) =
  local (\r -> r{envRunProps = (envRunProps r){rStrikethrough=Just SingleStrike}}) $
  inlinesToParElems ils
inlineToParElems (Superscript ils) =
  local (\r -> r{envRunProps = (envRunProps r){rBaseline=Just 30000}}) $
  inlinesToParElems ils
inlineToParElems (Subscript ils) =
  local (\r -> r{envRunProps = (envRunProps r){rBaseline=Just (-25000)}}) $
  inlinesToParElems ils
inlineToParElems (SmallCaps ils) =
  local (\r -> r{envRunProps = (envRunProps r){rCap = Just SmallCapitals}}) $
  inlinesToParElems ils
inlineToParElems Space = inlineToParElems (Str " ")
inlineToParElems SoftBreak = inlineToParElems (Str " ")
inlineToParElems LineBreak = return [Break]
inlineToParElems (Link _ ils (url, title)) = do
  local (\r ->r{envRunProps = (envRunProps r){rLink = Just (url, title)}}) $
    inlinesToParElems ils
inlineToParElems (Code _ str) = do
  local (\r ->r{envRunProps = (envRunProps r){rPropCode = True}}) $
    inlineToParElems $ Str str
inlineToParElems (Math mathtype str) =
  return [MathElem mathtype (TeXString str)]
inlineToParElems (Note blks) = do
  notes <- gets stNoteIds
  let maxNoteId = case M.keys notes of
        [] -> 0
        lst -> maximum lst
      curNoteId = maxNoteId + 1
  modify $ \st -> st { stNoteIds = M.insert curNoteId blks notes }
  inlineToParElems $ Superscript [Str $ show curNoteId]
inlineToParElems (Span _ ils) = concatMapM inlineToParElems ils
inlineToParElems (RawInline _ _) = return []
inlineToParElems _ = return []

isListType :: Block -> Bool
isListType (OrderedList _ _) = True
isListType (BulletList _) = True
isListType (DefinitionList _) = True
isListType _ = False

registerAnchorId :: PandocMonad m => String -> P m ()
registerAnchorId anchor = do
  anchorMap <- gets stAnchorMap
  slideId <- asks envCurSlideId
  unless (null anchor) $
    modify $ \st -> st {stAnchorMap = M.insert anchor slideId anchorMap}

blockToParagraphs :: PandocMonad m => Block -> P m [Paragraph]
blockToParagraphs (Plain ils) = do
  parElems <- inlinesToParElems ils
  pProps <- asks envParaProps
  return [Paragraph pProps parElems]
blockToParagraphs (Para ils) = do
  parElems <- inlinesToParElems ils
  pProps <- asks envParaProps
  return [Paragraph pProps parElems]
blockToParagraphs (LineBlock ilsList) = do
  parElems <- inlinesToParElems $ intercalate [LineBreak] ilsList
  pProps <- asks envParaProps
  return [Paragraph pProps parElems]
-- TODO: work out the attributes
blockToParagraphs (CodeBlock attr str) =
  local (\r -> r{envParaProps = def{pPropMarginLeft = Just 100}}) $
  blockToParagraphs $ Para [Code attr str]
-- We can't yet do incremental lists, but we should render a
-- (BlockQuote List) as a list to maintain compatibility with other
-- formats.
blockToParagraphs (BlockQuote (blk : blks)) | isListType blk = do
  ps  <- blockToParagraphs blk
  ps' <- blockToParagraphs $ BlockQuote blks
  return $ ps ++ ps'
blockToParagraphs (BlockQuote blks) =
  local (\r -> r{ envParaProps = (envParaProps r){pPropMarginLeft = Just 100}
                , envRunProps = (envRunProps r){rPropForceSize = Just blockQuoteSize}})$
  concatMapM blockToParagraphs blks
-- TODO: work out the format
blockToParagraphs (RawBlock _ _) = return []
blockToParagraphs (Header _ (ident, _, _) ils) = do
  -- Note that this function only deals with content blocks, so it
  -- will only touch headers that are above the current slide level --
  -- slides at or below the slidelevel will be taken care of by
  -- `blocksToSlide'`. We have the register anchors in both of them.
  registerAnchorId ident
  -- we set the subeader to bold
  parElems <- local (\e->e{envRunProps = (envRunProps e){rPropBold=True}}) $
              inlinesToParElems ils
  -- and give it a bit of space before it.
  return [Paragraph def{pPropSpaceBefore = Just 30} parElems]
blockToParagraphs (BulletList blksLst) = do
  pProps <- asks envParaProps
  let lvl = pPropLevel pProps
  local (\env -> env{ envInList = True
                    , envParaProps = pProps{ pPropLevel = lvl + 1
                                           , pPropBullet = Just Bullet
                                           , pPropMarginLeft = Nothing
                                           }}) $
    concatMapM multiParBullet blksLst
blockToParagraphs (OrderedList listAttr blksLst) = do
  pProps <- asks envParaProps
  let lvl = pPropLevel pProps
  local (\env -> env{ envInList = True
                    , envParaProps = pProps{ pPropLevel = lvl + 1
                                           , pPropBullet = Just (AutoNumbering listAttr)
                                           , pPropMarginLeft = Nothing
                                           }}) $
    concatMapM multiParBullet blksLst
blockToParagraphs (DefinitionList entries) = do
  let go :: PandocMonad m => ([Inline], [[Block]]) -> P m [Paragraph]
      go (ils, blksLst) = do
        term <-blockToParagraphs $ Para [Strong ils]
        -- For now, we'll treat each definition term as a
        -- blockquote. We can extend this further later.
        definition <- concatMapM (blockToParagraphs . BlockQuote) blksLst
        return $ term ++ definition
  concatMapM go entries
blockToParagraphs (Div (_, ("notes" : []), _) _) = return []
blockToParagraphs (Div _ blks)  = concatMapM blockToParagraphs blks
blockToParagraphs blk = do
  P.report $ BlockNotRendered blk
  return []

-- Make sure the bullet env gets turned off after the first para.
multiParBullet :: PandocMonad m => [Block] -> P m [Paragraph]
multiParBullet [] = return []
multiParBullet (b:bs) = do
  pProps <- asks envParaProps
  p <- blockToParagraphs b
  ps <- local (\env -> env{envParaProps = pProps{pPropBullet = Nothing}}) $
    concatMapM blockToParagraphs bs
  return $ p ++ ps

cellToParagraphs :: PandocMonad m => Alignment -> TableCell -> P m [Paragraph]
cellToParagraphs algn tblCell = do
  paras <- mapM (blockToParagraphs) tblCell
  let alignment = case algn of
        AlignLeft -> Just AlgnLeft
        AlignRight -> Just AlgnRight
        AlignCenter -> Just AlgnCenter
        AlignDefault -> Nothing
      paras' = map (map (\p -> p{paraProps = (paraProps p){pPropAlign = alignment}})) paras
  return $ concat paras'

rowToParagraphs :: PandocMonad m => [Alignment] -> [TableCell] -> P m [[Paragraph]]
rowToParagraphs algns tblCells = do
  -- We have to make sure we have the right number of alignments
  let pairs = zip (algns ++ repeat AlignDefault) tblCells
  mapM (\(a, tc) -> cellToParagraphs a tc) pairs

blockToShape :: PandocMonad m => Block -> P m Shape
blockToShape (Plain (il:_)) | Image attr ils (url, _) <- il =
      Pic def url attr <$> (inlinesToParElems ils)
blockToShape (Para (il:_))  | Image attr ils (url, _) <- il =
      Pic def url attr <$> (inlinesToParElems ils)
blockToShape (Plain (il:_)) | Link _ (il':_) target <- il
                            , Image attr ils (url, _) <- il' =
      Pic def{picPropLink = Just target} url attr <$> (inlinesToParElems ils)
blockToShape (Para (il:_))  | Link _ (il':_) target <- il
                            , Image attr ils (url, _) <- il' =
      Pic def{picPropLink = Just target} url attr <$> (inlinesToParElems ils)
blockToShape (Table caption algn _ hdrCells rows) = do
  caption' <- inlinesToParElems caption
  (pageWidth, _) <- asks envPresentationSize
  hdrCells' <- rowToParagraphs algn hdrCells
  rows' <- mapM (rowToParagraphs algn) rows
  let tblPr = if null hdrCells
              then TableProps { tblPrFirstRow = False
                              , tblPrBandRow = True
                              }
              else TableProps { tblPrFirstRow = True
                              , tblPrBandRow = True
                              }
      colWidths = if null hdrCells
                 then case rows of
                        r : _ | not (null r) -> replicate (length r) $
                                                (pageWidth - (2 * hardcodedTableMargin))`div` (toInteger $ length r)
                        -- satisfy the compiler. This is the same as
                        -- saying that rows is empty, but the compiler
                        -- won't understand that `[]` exhausts the
                        -- alternatives.
                        _ -> []
                 else replicate (length hdrCells) $
                      (pageWidth - (2 * hardcodedTableMargin)) `div` (toInteger $ length hdrCells)

  return $ GraphicFrame [Tbl tblPr colWidths hdrCells' rows'] caption'
blockToShape blk = TextBox <$> blockToParagraphs blk

blocksToShapes :: PandocMonad m => [Block] -> P m [Shape]
blocksToShapes blks = combineShapes <$> mapM blockToShape blks

isImage :: Inline -> Bool
isImage (Image _ _ _) = True
isImage (Link _ ((Image _ _ _) : _) _) = True
isImage _ = False

splitBlocks' :: Monad m => [Block] -> [[Block]] -> [Block] -> P m [[Block]]
splitBlocks' cur acc [] = return $ acc ++ (if null cur then [] else [cur])
splitBlocks' cur acc (HorizontalRule : blks) =
  splitBlocks' [] (acc ++ (if null cur then [] else [cur])) blks
splitBlocks' cur acc (h@(Header n _ _) : blks) = do
  slideLevel <- asks envSlideLevel
  case compare n slideLevel of
    LT -> splitBlocks' [] (acc ++ (if null cur then [] else [cur]) ++ [[h]]) blks
    EQ -> splitBlocks' [h] (acc ++ (if null cur then [] else [cur])) blks
    GT -> splitBlocks' (cur ++ [h]) acc blks
-- `blockToParagraphs` treats Plain and Para the same, so we can save
-- some code duplication by treating them the same here.
splitBlocks' cur acc ((Plain ils) : blks) = splitBlocks' cur acc ((Para ils) : blks)
splitBlocks' cur acc ((Para (il:ils)) : blks) | isImage il = do
  slideLevel <- asks envSlideLevel
  case cur of
    (Header n _ _) : [] | n == slideLevel ->
                            splitBlocks' []
                            (acc ++ [cur ++ [Para [il]]])
                            (if null ils then blks else (Para ils) : blks)
    _ -> splitBlocks' []
         (acc ++ (if null cur then [] else [cur]) ++ [[Para [il]]])
         (if null ils then blks else (Para ils) : blks)
splitBlocks' cur acc (tbl@(Table _ _ _ _ _) : blks) = do
  slideLevel <- asks envSlideLevel
  case cur of
    (Header n _ _) : [] | n == slideLevel ->
                            splitBlocks' [] (acc ++ [cur ++ [tbl]]) blks
    _ ->  splitBlocks' [] (acc ++ (if null cur then [] else [cur]) ++ [[tbl]]) blks
splitBlocks' cur acc (d@(Div (_, classes, _) _): blks) | "columns" `elem` classes =  do
  slideLevel <- asks envSlideLevel
  case cur of
    (Header n _ _) : [] | n == slideLevel ->
                            splitBlocks' [] (acc ++ [cur ++ [d]]) blks
    _ ->  splitBlocks' [] (acc ++ (if null cur then [] else [cur]) ++ [[d]]) blks
splitBlocks' cur acc (blk : blks) = splitBlocks' (cur ++ [blk]) acc blks

splitBlocks :: Monad m => [Block] -> P m [[Block]]
splitBlocks = splitBlocks' [] []

blocksToSlide' :: PandocMonad m => Int -> [Block] -> P m Slide
blocksToSlide' lvl ((Header n (ident, _, _) ils) : blks)
  | n < lvl = do
      registerAnchorId ident
      hdr <- inlinesToParElems ils
      return $ TitleSlide {titleSlideHeader = hdr}
  | n == lvl = do
      registerAnchorId ident
      hdr <- inlinesToParElems ils
      -- Now get the slide without the header, and then add the header
      -- in.
      slide <- blocksToSlide' lvl blks
      return $ case slide of
        ContentSlide _ cont          -> ContentSlide hdr cont
        TwoColumnSlide _ contL contR -> TwoColumnSlide hdr contL contR
        slide'                       -> slide'
blocksToSlide' _ (blk : blks)
  | Div (_, classes, _) divBlks <- blk
  , "columns" `elem` classes
  , (Div (_, clsL, _) blksL) : (Div (_, clsR, _) blksR) : remaining <- divBlks
  , "column" `elem` clsL, "column" `elem` clsR = do
      unless (null blks)
        (mapM (P.report . BlockNotRendered) blks >> return ())
      unless (null remaining)
        (mapM (P.report . BlockNotRendered) remaining >> return ())
      shapesL <- blocksToShapes blksL
      shapesR <- blocksToShapes blksR
      return $ TwoColumnSlide { twoColumnSlideHeader = []
                              , twoColumnSlideLeft = shapesL
                              , twoColumnSlideRight = shapesR
                              }
blocksToSlide' _ (blk : blks) = do
      inNoteSlide <- asks envInNoteSlide
      shapes <- if inNoteSlide
                then forceFontSize noteSize $ blocksToShapes (blk : blks)
                else blocksToShapes (blk : blks)
      return $ ContentSlide { contentSlideHeader = []
                            , contentSlideContent = shapes
                            }
blocksToSlide' _ [] = return $ ContentSlide { contentSlideHeader = []
                                            , contentSlideContent = []
                                            }

blocksToSlide :: PandocMonad m => [Block] -> P m Slide
blocksToSlide blks = do
  slideLevel <- asks envSlideLevel
  blocksToSlide' slideLevel blks

makeNoteEntry :: Int -> [Block] -> [Block]
makeNoteEntry n blks =
  let enum = Str (show n ++ ".")
  in
    case blks of
      (Para ils : blks') -> (Para $ enum : Space : ils) : blks'
      _ -> (Para [enum]) : blks

forceFontSize :: PandocMonad m => Pixels -> P m a -> P m a
forceFontSize px x = do
  rpr <- asks envRunProps
  local (\r -> r {envRunProps = rpr{rPropForceSize = Just px}}) x

-- We leave these as blocks because we will want to include them in
-- the TOC.
makeNotesSlideBlocks :: PandocMonad m => P m [Block]
makeNotesSlideBlocks = do
  noteIds <- gets stNoteIds
  slideLevel <- asks envSlideLevel
  meta <- asks envMetadata
  -- Get identifiers so we can give the notes section a unique ident.
  anchorSet <- M.keysSet <$> gets stAnchorMap
  if M.null noteIds
    then return []
    else do let title = case lookupMeta "notes-title" meta of
                  Just val -> metaValueToInlines val
                  Nothing  -> [Str "Notes"]
                ident = Shared.uniqueIdent title anchorSet
                hdr = Header slideLevel (ident, [], []) title
            blks <- return $
                    concatMap (\(n, bs) -> makeNoteEntry n bs) $
                    M.toList noteIds
            return $ hdr : blks

getMetaSlide :: PandocMonad m => P m (Maybe Slide)
getMetaSlide  = do
  meta <- asks envMetadata
  title <- inlinesToParElems $ docTitle meta
  subtitle <- inlinesToParElems $
    case lookupMeta "subtitle" meta of
      Just (MetaString s)           -> [Str s]
      Just (MetaInlines ils)        -> ils
      Just (MetaBlocks [Plain ils]) -> ils
      Just (MetaBlocks [Para ils])  -> ils
      _                             -> []
  authors <- mapM inlinesToParElems $ docAuthors meta
  date <- inlinesToParElems $ docDate meta
  if null title && null subtitle && null authors && null date
    then return Nothing
    else return $ Just $ MetadataSlide { metadataSlideTitle = title
                                       , metadataSlideSubtitle = subtitle
                                       , metadataSlideAuthors = authors
                                       , metadataSlideDate = date
                                       }

-- adapted from the markdown writer
elementToListItem :: PandocMonad m => Shared.Element -> P m [Block]
elementToListItem (Shared.Sec lev _nums (ident,_,_) headerText subsecs) = do
  opts <- asks envOpts
  let headerLink = if null ident
                   then walk Shared.deNote headerText
                   else [Link nullAttr (walk Shared.deNote headerText)
                          ('#':ident, "")]
  listContents <- if null subsecs || lev >= writerTOCDepth opts
                  then return []
                  else mapM elementToListItem subsecs
  return [Plain headerLink, BulletList listContents]
elementToListItem (Shared.Blk _) = return []

makeTOCSlide :: PandocMonad m => [Block] -> P m Slide
makeTOCSlide blks = do
  contents <- BulletList <$> mapM elementToListItem (Shared.hierarchicalize blks)
  meta <- asks envMetadata
  slideLevel <- asks envSlideLevel
  let tocTitle = case lookupMeta "toc-title" meta of
                   Just val -> metaValueToInlines val
                   Nothing  -> [Str "Table of Contents"]
      hdr = Header slideLevel nullAttr tocTitle
  sld <- blocksToSlide [hdr, contents]
  return sld

blocksToPresentation :: PandocMonad m => [Block] -> P m Presentation
blocksToPresentation blks = do
  opts <- asks envOpts
  let metadataStartNum = 1
  metadataslides <- maybeToList <$> getMetaSlide
  let tocStartNum = metadataStartNum + length metadataslides
  -- As far as I can tell, if we want to have a variable-length toc in
  -- the future, we'll have to make it twice. Once to get the length,
  -- and a second time to include the notes slide. We can't make the
  -- notes slide before the body slides because we need to know if
  -- there are notes, and we can't make either before the toc slide,
  -- because we need to know its length to get slide numbers right.
  --
  -- For now, though, since the TOC slide is only length 1, if it
  -- exists, we'll just get the length, and then come back to make the
  -- slide later
  let tocSlidesLength = if writerTableOfContents opts then 1 else 0
  let bodyStartNum = tocStartNum + tocSlidesLength
  blksLst <- splitBlocks blks
  bodyslides <- mapM
                (\(bs, n) -> local (\st -> st{envCurSlideId = n}) (blocksToSlide bs))
                (zip blksLst [bodyStartNum..])
  let noteStartNum = bodyStartNum + length bodyslides
  notesSlideBlocks <- makeNotesSlideBlocks
  -- now we come back and make the real toc...
  tocSlides <- if writerTableOfContents opts
               then do toc <- makeTOCSlide $ blks ++ notesSlideBlocks
                       return [toc]
               else return []
  -- ... and the notes slide. We test to see if the blocks are empty,
  -- because we don't want to make an empty slide.
  notesSlides <- if null notesSlideBlocks
                 then return []
                 else do notesSlide <- local
                           (\env -> env { envCurSlideId = noteStartNum
                                        , envInNoteSlide = True
                                        })
                           (blocksToSlide $ notesSlideBlocks)
                         return [notesSlide]
  return $
    Presentation $
    metadataslides ++ tocSlides ++ bodyslides ++ notesSlides

--------------------------------------------------------------------

copyFileToArchive :: PandocMonad m => Archive -> FilePath -> P m Archive
copyFileToArchive arch fp = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  case findEntryByPath fp refArchive `mplus` findEntryByPath fp distArchive of
    Nothing -> fail $ fp ++ " missing in reference file"
    Just e -> return $ addEntryToArchive e arch

inheritedPatterns :: [Pattern]
inheritedPatterns = map compile [ "_rels/.rels"
                                , "docProps/app.xml"
                                , "docProps/core.xml"
                                , "ppt/slideLayouts/slideLayout*.xml"
                                , "ppt/slideLayouts/_rels/slideLayout*.xml.rels"
                                , "ppt/slideMasters/slideMaster1.xml"
                                , "ppt/slideMasters/_rels/slideMaster1.xml.rels"
                                , "ppt/theme/theme1.xml"
                                , "ppt/theme/_rels/theme1.xml.rels"
                                , "ppt/presProps.xml"
                                , "ppt/viewProps.xml"
                                , "ppt/tableStyles.xml"
                                , "ppt/media/image*"
                                ]

patternToFilePaths :: PandocMonad m => Pattern -> P m [FilePath]
patternToFilePaths pat = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive

  let archiveFiles = filesInArchive refArchive `union` filesInArchive distArchive
  return $ filter (match pat) archiveFiles

patternsToFilePaths :: PandocMonad m => [Pattern] -> P m [FilePath]
patternsToFilePaths pats = concat <$> mapM patternToFilePaths pats

-- Here are the files we'll require to make a Powerpoint document. If
-- any of these are missing, we should error out of our build.
requiredFiles :: [FilePath]
requiredFiles = [ "_rels/.rels"
                 , "docProps/app.xml"
                 , "docProps/core.xml"
                 , "ppt/presProps.xml"
                 , "ppt/slideLayouts/slideLayout1.xml"
                 , "ppt/slideLayouts/_rels/slideLayout1.xml.rels"
                 , "ppt/slideLayouts/slideLayout2.xml"
                 , "ppt/slideLayouts/_rels/slideLayout2.xml.rels"
                 , "ppt/slideLayouts/slideLayout3.xml"
                 , "ppt/slideLayouts/_rels/slideLayout3.xml.rels"
                 , "ppt/slideLayouts/slideLayout4.xml"
                 , "ppt/slideLayouts/_rels/slideLayout4.xml.rels"
                 , "ppt/slideMasters/slideMaster1.xml"
                 , "ppt/slideMasters/_rels/slideMaster1.xml.rels"
                 , "ppt/theme/theme1.xml"
                 , "ppt/viewProps.xml"
                 , "ppt/tableStyles.xml"
                 ]


presentationToArchive :: PandocMonad m => Presentation -> P m Archive
presentationToArchive p@(Presentation slides) = do
  filePaths <- patternsToFilePaths inheritedPatterns

  -- make sure all required files are available:
  let missingFiles = filter (\fp -> not (fp `elem` filePaths)) requiredFiles
  unless (null missingFiles)
    (throwError $
      PandocSomeError $
      "The following required files are missing:\n" ++
      (unlines $ map ("  " ++) missingFiles)
    )

  newArch' <- foldM copyFileToArchive emptyArchive filePaths
  -- presentation entry and rels. We have to do the rels first to make
  -- sure we know the correct offset for the rIds.
  presEntry <- presentationToPresEntry p
  presRelsEntry <- presentationToRelsEntry p
  slideEntries <- mapM (\(s, n) -> slideToEntry s n) $ zip slides [1..]
  slideRelEntries <- mapM (\(s,n) -> slideToSlideRelEntry s n) $ zip slides [1..]
  -- These have to come after everything, because they need the info
  -- built up in the state.
  mediaEntries <- makeMediaEntries
  contentTypesEntry <- presentationToContentTypes p >>= contentTypesToEntry
  -- fold everything into our inherited archive and return it.
  return $ foldr addEntryToArchive newArch' $
    slideEntries ++
    slideRelEntries ++
    mediaEntries ++
    [contentTypesEntry, presEntry, presRelsEntry]

--------------------------------------------------

combineShapes :: [Shape] -> [Shape]
combineShapes [] = []
combineShapes (s : []) = [s]
combineShapes (pic@(Pic _ _ _ _) : ss) = pic : combineShapes ss
combineShapes ((TextBox []) : ss) = combineShapes ss
combineShapes (s : TextBox [] : ss) = combineShapes (s : ss)
combineShapes ((TextBox (p:ps)) : (TextBox (p':ps')) : ss) =
  combineShapes $ TextBox ((p:ps) ++ (p':ps')) : ss
combineShapes (s:ss) = s : combineShapes ss

--------------------------------------------------

getLayout :: PandocMonad m => Slide -> P m Element
getLayout slide = do
  let layoutpath = case slide of
        (MetadataSlide _ _ _ _) -> "ppt/slideLayouts/slideLayout1.xml"
        (TitleSlide _)          -> "ppt/slideLayouts/slideLayout3.xml"
        (ContentSlide _ _)      -> "ppt/slideLayouts/slideLayout2.xml"
        (TwoColumnSlide _ _ _)    -> "ppt/slideLayouts/slideLayout4.xml"
  distArchive <- asks envDistArchive
  root <- case findEntryByPath layoutpath distArchive of
        Just e -> case parseXMLDoc $ UTF8.toStringLazy $ fromEntry e of
                    Just element -> return $ element
                    Nothing      -> throwError $
                                    PandocSomeError $
                                    layoutpath ++ " corrupt in reference file"
        Nothing -> throwError $
                   PandocSomeError $
                   layoutpath ++ " missing in reference file"
  return root

shapeHasName :: NameSpaces -> String -> Element -> Bool
shapeHasName ns name element
  | Just nvSpPr <- findChild (elemName ns "p" "nvSpPr") element
  , Just cNvPr <- findChild (elemName ns "p" "cNvPr") nvSpPr
  , Just nm <- findAttr (QName "name" Nothing Nothing) cNvPr =
      nm == name
  | otherwise = False

shapeHasId :: NameSpaces -> String -> Element -> Bool
shapeHasId ns ident element
  | Just nvSpPr <- findChild (elemName ns "p" "nvSpPr") element
  , Just cNvPr <- findChild (elemName ns "p" "cNvPr") nvSpPr
  , Just nm <- findAttr (QName "id" Nothing Nothing) cNvPr =
      nm == ident
  | otherwise = False

-- The content shape in slideLayout2 (Title/Content) has id=3 In
-- slideLayout4 (two column) the left column is id=3, and the right
-- column is id=4.
getContentShape :: PandocMonad m => NameSpaces -> Element -> P m Element
getContentShape ns spTreeElem
  | isElem ns "p" "spTree" spTreeElem =
   case filterChild
        (\e -> (isElem ns "p" "sp" e) && (shapeHasId ns "3" e))
        spTreeElem
   of
     Just e -> return e
     Nothing -> throwError $
                PandocSomeError $
                "Could not find shape for Powerpoint content"
getContentShape _ _ = throwError $
                      PandocSomeError $
                      "Attempted to find content on non shapeTree"

getShapeDimensions :: NameSpaces
                   -> Element
                   -> Maybe ((Integer, Integer), (Integer, Integer))
getShapeDimensions ns element
  | isElem ns "p" "sp" element = do
      spPr <- findChild (elemName ns "p" "spPr") element
      xfrm <- findChild (elemName ns "a" "xfrm") spPr
      off <- findChild (elemName ns "a" "off") xfrm
      xS <- findAttr (QName "x" Nothing Nothing) off
      yS <- findAttr (QName "y" Nothing Nothing) off
      ext <- findChild (elemName ns "a" "ext") xfrm
      cxS <- findAttr (QName "cx" Nothing Nothing) ext
      cyS <- findAttr (QName "cy" Nothing Nothing) ext
      (x, _) <- listToMaybe $ reads xS
      (y, _) <- listToMaybe $ reads yS
      (cx, _) <- listToMaybe $ reads cxS
      (cy, _) <- listToMaybe $ reads cyS
      return $ ((x `div` 12700, y `div` 12700), (cx `div` 12700, cy `div` 12700))
  | otherwise = Nothing


getMasterShapeDimensionsById :: String
                             -> Element
                             -> Maybe ((Integer, Integer), (Integer, Integer))
getMasterShapeDimensionsById ident master = do
  let ns = elemToNameSpaces master
  cSld <- findChild (elemName ns "p" "cSld") master
  spTree <- findChild (elemName ns "p" "spTree") cSld
  sp <- filterChild (\e -> (isElem ns "p" "sp" e) && (shapeHasId ns ident e)) spTree
  getShapeDimensions ns sp

getContentShapeSize :: PandocMonad m
                    => NameSpaces
                    -> Element
                    -> Element
                    -> P m ((Integer, Integer), (Integer, Integer))
getContentShapeSize ns layout master
  | isElem ns "p" "sldLayout" layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      sp  <- getContentShape ns spTree
      case getShapeDimensions ns sp of
        Just sz -> return sz
        Nothing -> do let mbSz =
                            findChild (elemName ns "p" "nvSpPr") sp >>=
                            findChild (elemName ns "p" "cNvPr") >>=
                            findAttr (QName "id" Nothing Nothing) >>=
                            flip getMasterShapeDimensionsById master
                      case mbSz of
                        Just sz' -> return sz'
                        Nothing -> throwError $
                                   PandocSomeError $
                                   "Couldn't find necessary content shape size"
getContentShapeSize _ _ _ = throwError $
                            PandocSomeError $
                            "Attempted to find content shape size in non-layout"

replaceNamedChildren :: NameSpaces
                   -> String
                   -> String
                   -> [Element]
                   -> Element
                   -> Element
replaceNamedChildren ns prefix name newKids element =
  element { elContent = concat $ fun True $ elContent element }
  where
    fun :: Bool -> [Content] -> [[Content]]
    fun _ [] = []
    fun switch ((Elem e) : conts) | isElem ns prefix name e =
                                      if switch
                                      then (map Elem $ newKids) : fun False conts
                                      else fun False conts
    fun switch (cont : conts) = [cont] : fun switch conts

----------------------------------------------------------------

registerLink :: PandocMonad m => (URL, String) -> P m Int
registerLink link = do
  curSlideId <- asks envCurSlideId
  linkReg <- gets stLinkIds
  mediaReg <- gets stMediaIds
  let maxLinkId = case M.lookup curSlideId linkReg of
        Just mp -> case M.keys mp of
          [] -> 1
          ks -> maximum ks
        Nothing -> 1
      maxMediaId = case M.lookup curSlideId mediaReg of
        Just [] -> 1
        Just mInfos -> maximum $ map mInfoLocalId mInfos
        Nothing -> 1
      maxId = max maxLinkId maxMediaId
      slideLinks = case M.lookup curSlideId linkReg of
        Just mp -> M.insert (maxId + 1) link mp
        Nothing -> M.singleton (maxId + 1) link
  modify $ \st -> st{ stLinkIds = M.insert curSlideId slideLinks linkReg}
  return $ maxId + 1

registerMedia :: PandocMonad m => FilePath -> [ParaElem] -> P m MediaInfo
registerMedia fp caption = do
  curSlideId <- asks envCurSlideId
  linkReg <- gets stLinkIds
  mediaReg <- gets stMediaIds
  globalIds <- gets stMediaGlobalIds
  let maxLinkId = case M.lookup curSlideId linkReg of
        Just mp -> case M.keys mp of
          [] -> 1
          ks -> maximum ks
        Nothing -> 1
      maxMediaId = case M.lookup curSlideId mediaReg of
        Just [] -> 1
        Just mInfos -> maximum $ map mInfoLocalId mInfos
        Nothing -> 1
      maxLocalId = max maxLinkId maxMediaId

      maxGlobalId = case M.elems globalIds of
        [] -> 0
        ids -> maximum ids

  (imgBytes, mbMt) <- P.fetchItem fp
  let imgExt = (mbMt >>= extensionFromMimeType >>= (\x -> return $ '.':x))
               <|>
               case imageType imgBytes of
                 Just Png  -> Just ".png"
                 Just Jpeg -> Just ".jpeg"
                 Just Gif  -> Just ".gif"
                 Just Pdf  -> Just ".pdf"
                 Just Eps  -> Just ".eps"
                 Just Svg  -> Just ".svg"
                 Nothing   -> Nothing

  let newGlobalId = case M.lookup fp globalIds of
        Just ident -> ident
        Nothing    -> maxGlobalId + 1

  let newGlobalIds = M.insert fp newGlobalId globalIds

  let mediaInfo = MediaInfo { mInfoFilePath = fp
                            , mInfoLocalId = maxLocalId + 1
                            , mInfoGlobalId = newGlobalId
                            , mInfoMimeType = mbMt
                            , mInfoExt = imgExt
                            , mInfoCaption = (not . null) caption
                            }

  let slideMediaInfos = case M.lookup curSlideId mediaReg of
        Just minfos -> mediaInfo : minfos
        Nothing     -> [mediaInfo]


  modify $ \st -> st{ stMediaIds = M.insert curSlideId slideMediaInfos mediaReg
                    , stMediaGlobalIds = newGlobalIds
                    }
  return mediaInfo

makeMediaEntry :: PandocMonad m => MediaInfo -> P m Entry
makeMediaEntry mInfo = do
  epochtime <- (floor . utcTimeToPOSIXSeconds) <$> asks envUTCTime
  (imgBytes, _) <- P.fetchItem (mInfoFilePath mInfo)
  let ext = case mInfoExt mInfo of
              Just e -> e
              Nothing -> ""
  let fp = "ppt/media/image" ++ (show $ mInfoGlobalId mInfo) ++ ext
  return $ toEntry fp epochtime $ BL.fromStrict imgBytes

makeMediaEntries :: PandocMonad m => P m [Entry]
makeMediaEntries = do
  mediaInfos <- gets stMediaIds
  let allInfos = mconcat $ M.elems mediaInfos
  mapM makeMediaEntry allInfos

-- -- | Scales the image to fit the page
-- -- sizes are passed in emu
-- fitToPage' :: (Double, Double)  -- image size in emu
--            -> Integer           -- pageWidth
--            -> Integer           -- pageHeight
--            -> (Integer, Integer) -- imagesize
-- fitToPage' (x, y) pageWidth pageHeight
--   -- Fixes width to the page width and scales the height
--   | x <= fromIntegral pageWidth && y <= fromIntegral pageHeight =
--       (floor x, floor y)
--   | x / fromIntegral pageWidth > y / fromIntegral pageWidth =
--       (pageWidth, floor $ ((fromIntegral pageWidth) / x) * y)
--   | otherwise =
--       (floor $ ((fromIntegral pageHeight) / y) * x, pageHeight)

-- positionImage :: (Double, Double) -> Integer -> Integer -> (Integer, Integer)
-- positionImage (x, y) pageWidth pageHeight =
--   let (x', y') = fitToPage' (x, y) pageWidth pageHeight
--   in
--     ((pageWidth - x') `div` 2, (pageHeight - y') `div`  2)

getMaster :: PandocMonad m => P m Element
getMaster = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  parseXml refArchive distArchive "ppt/slideMasters/slideMaster1.xml"

-- We want to get the header dimensions, so we can make sure that the
-- image goes underneath it. We only use this in a content slide if it
-- has a header.

-- getHeaderSize :: PandocMonad m => P m ((Integer, Integer), (Integer, Integer))
-- getHeaderSize = do
--   master <- getMaster
--   let ns = elemToNameSpaces master
--       sps = [master] >>=
--             findChildren (elemName ns "p" "cSld") >>=
--             findChildren (elemName ns "p" "spTree") >>=
--             findChildren (elemName ns "p" "sp")
--       mbXfrm =
--         listToMaybe (filter (shapeHasName ns "Title Placeholder 1") sps) >>=
--         findChild (elemName ns "p" "spPr") >>=
--         findChild (elemName ns "a" "xfrm")
--       xoff = mbXfrm >>=
--              findChild (elemName ns "a" "off") >>=
--              findAttr (QName "x" Nothing Nothing) >>=
--              (listToMaybe . (\s -> reads s :: [(Integer, String)]))
--       yoff = mbXfrm >>=
--              findChild (elemName ns "a" "off") >>=
--              findAttr (QName "y" Nothing Nothing) >>=
--              (listToMaybe . (\s -> reads s :: [(Integer, String)]))
--       xext = mbXfrm >>=
--              findChild (elemName ns "a" "ext") >>=
--              findAttr (QName "cx" Nothing Nothing) >>=
--              (listToMaybe . (\s -> reads s :: [(Integer, String)]))
--       yext = mbXfrm >>=
--              findChild (elemName ns "a" "ext") >>=
--              findAttr (QName "cy" Nothing Nothing) >>=
--              (listToMaybe . (\s -> reads s :: [(Integer, String)]))
--       off = case xoff of
--               Just (xoff', _) | Just (yoff',_) <- yoff -> (xoff', yoff')
--               _                               -> (1043490, 1027664)
--       ext = case xext of
--               Just (xext', _) | Just (yext',_) <- yext -> (xext', yext')
--               _                               -> (7024744, 1143000)
--   return $ (off, ext)

-- Hard-coded for now
-- captionPosition :: ((Integer, Integer), (Integer, Integer))
-- captionPosition = ((457200, 6061972), (8229600, 527087))

captionHeight :: Integer
captionHeight = 40

createCaption :: PandocMonad m
              => ((Integer, Integer), (Integer, Integer))
              -> [ParaElem]
              -> P m Element
createCaption contentShapeDimensions paraElements = do
  let para = Paragraph def{pPropAlign = Just AlgnCenter} paraElements
  elements <- mapM paragraphToElement [para]
  let ((x, y), (cx, cy)) = contentShapeDimensions
  let txBody = mknode "p:txBody" [] $
               [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] ++ elements
  return $
    mknode "p:sp" [] [ mknode "p:nvSpPr" []
                       [ mknode "p:cNvPr" [("id","1"), ("name","TextBox 3")] ()
                       , mknode "p:cNvSpPr" [("txBox", "1")] ()
                       , mknode "p:nvPr" [] ()
                       ]
                     , mknode "p:spPr" []
                       [ mknode "a:xfrm" []
                         [ mknode "a:off" [("x", show $ 12700 * x),
                                           ("y", show $ 12700 * (y + cy - captionHeight))] ()
                         , mknode "a:ext" [("cx", show $ 12700 * cx),
                                           ("cy", show $ 12700 * captionHeight)] ()
                         ]
                       , mknode "a:prstGeom" [("prst", "rect")]
                         [ mknode "a:avLst" [] ()
                         ]
                       , mknode "a:noFill" [] ()
                       ]
                     , txBody
                     ]

makePicElements :: PandocMonad m
                => Element
                -> PicProps
                -> MediaInfo
                -> Text.Pandoc.Definition.Attr
                -> [ParaElem]
                -> P m [Element]
makePicElements layout picProps mInfo _ alt = do
  opts <- asks envOpts
  (pageWidth, pageHeight) <- asks envPresentationSize
  -- hasHeader <- asks envSlideHasHeader
  let hasCaption = mInfoCaption mInfo
  (imgBytes, _) <- P.fetchItem (mInfoFilePath mInfo)
  let (pxX, pxY) = case imageSize opts imgBytes of
        Right sz -> sizeInPixels $ sz
        Left _   -> sizeInPixels $ def
  master <- getMaster
  let ns = elemToNameSpaces layout
  ((x, y), (cx, cytmp)) <- getContentShapeSize ns layout master
                           `catchError`
                           (\_ -> return ((0, 0), (pageWidth, pageHeight)))

  let cy = if hasCaption then cytmp - captionHeight else cytmp

  let imgRatio = fromIntegral pxX / fromIntegral pxY :: Double
      boxRatio = fromIntegral cx / fromIntegral cy :: Double
      (dimX, dimY) = if imgRatio > boxRatio
                     then (fromIntegral cx, fromIntegral cx / imgRatio)
                     else (fromIntegral cy * imgRatio, fromIntegral cy)

      (dimX', dimY') = (round dimX * 12700, round dimY * 12700) :: (Integer, Integer)
      (xoff, yoff) = (fromIntegral x + (fromIntegral cx - dimX) / 2,
                      fromIntegral y + (fromIntegral cy - dimY) / 2)
      (xoff', yoff') = (round xoff * 12700, round yoff * 12700) :: (Integer, Integer)

  let cNvPicPr = mknode "p:cNvPicPr" [] $
                 mknode "a:picLocks" [("noGrp","1")
                                     ,("noChangeAspect","1")] ()
  -- cNvPr will contain the link information so we do that separately,
  -- and register the link if necessary.
  let cNvPrAttr = [("descr", mInfoFilePath mInfo), ("id","0"),("name","Picture 1")]
  cNvPr <- case picPropLink picProps of
    Just link -> do idNum <- registerLink link
                    return $ mknode "p:cNvPr" cNvPrAttr $
                      mknode "a:hlinkClick" [("r:id", "rId" ++ show idNum)] ()
    Nothing   -> return $ mknode "p:cNvPr" cNvPrAttr ()
  let nvPicPr  = mknode "p:nvPicPr" []
                 [ cNvPr
                 , cNvPicPr
                 , mknode "p:nvPr" [] ()]
  let blipFill = mknode "p:blipFill" []
                 [ mknode "a:blip" [("r:embed", "rId" ++ (show $ mInfoLocalId mInfo))] ()
                 , mknode "a:stretch" [] $
                   mknode "a:fillRect" [] () ]
  let xfrm =    mknode "a:xfrm" []
                [ mknode "a:off" [("x",show xoff'), ("y",show yoff')] ()
                , mknode "a:ext" [("cx",show dimX')
                                 ,("cy",show dimY')] () ]
  let prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                 mknode "a:avLst" [] ()
  let ln =      mknode "a:ln" [("w","9525")]
                [ mknode "a:noFill" [] ()
                , mknode "a:headEnd" [] ()
                , mknode "a:tailEnd" [] () ]
  let spPr =    mknode "p:spPr" [("bwMode","auto")]
                [xfrm, prstGeom, mknode "a:noFill" [] (), ln]

  let picShape = mknode "p:pic" []
                 [ nvPicPr
                 , blipFill
                 , spPr ]

  -- And now, maybe create the caption:
  if hasCaption
    then do cap <- createCaption ((x, y), (cx, cytmp)) alt
            return [picShape, cap]
    else return [picShape]

-- Currently hardcoded, until I figure out how to make it dynamic.
blockQuoteSize :: Pixels
blockQuoteSize = 20

noteSize :: Pixels
noteSize = 18

paraElemToElement :: PandocMonad m => ParaElem -> P m Element
paraElemToElement Break = return $ mknode "a:br" [] ()
paraElemToElement (Run rpr s) = do
  let sizeAttrs = case rPropForceSize rpr of
                    Just n -> [("sz", (show $ n * 100))]
                    Nothing -> []
      attrs = sizeAttrs ++
        if rPropCode rpr
        then []
        else (if rPropBold rpr then [("b", "1")] else []) ++
             (if rPropItalics rpr then [("i", "1")] else []) ++
             (case rStrikethrough rpr of
                Just NoStrike     -> [("strike", "noStrike")]
                Just SingleStrike -> [("strike", "sngStrike")]
                Just DoubleStrike -> [("strike", "dblStrike")]
                Nothing -> []) ++
             (case rBaseline rpr of
                Just n -> [("baseline", show n)]
                Nothing -> []) ++
             (case rCap rpr of
                Just NoCapitals -> [("cap", "none")]
                Just SmallCapitals -> [("cap", "small")]
                Just AllCapitals -> [("cap", "all")]
                Nothing -> []) ++
             []
  linkProps <- case rLink rpr of
                 Just link -> do
                   idNum <- registerLink link
                   -- first we have to make sure that if it's an
                   -- anchor, it's in the anchor map. If not, there's
                   -- no link.
                   anchorMap <- gets stAnchorMap
                   return $ case link of
                     -- anchor with nothing in the map
                     ('#':target, _) | Nothing <- M.lookup target anchorMap ->
                       []
                     --  anchor that is in the map
                     ('#':_, _) ->
                       let linkAttrs =
                             [ ("r:id", "rId" ++ show idNum)
                             , ("action", "ppaction://hlinksldjump")
                             ]
                       in [mknode "a:hlinkClick" linkAttrs ()]
                     -- external
                     _ ->
                       let linkAttrs =
                             [ ("r:id", "rId" ++ show idNum)
                             ]
                       in [mknode "a:hlinkClick" linkAttrs ()]
                 Nothing -> return []
  let propContents = if rPropCode rpr
                     then [mknode "a:latin" [("typeface", "Courier")] ()]
                     else linkProps
  return $ mknode "a:r" [] [ mknode "a:rPr" attrs propContents
                           , mknode "a:t" [] s
                           ]
paraElemToElement (MathElem mathType texStr) = do
  res <- convertMath writeOMML mathType (unTeXString texStr)
  case res of
    Right r -> return $ mknode "a14:m" [] $ addMathInfo r
    Left (Str s) -> paraElemToElement (Run def s)
    Left _       -> throwError $ PandocShouldNeverHappenError "non-string math fallback"

-- This is a bit of a kludge -- really requires adding an option to
-- TeXMath, but since that's a different package, we'll do this one
-- step at a time.
addMathInfo :: Element -> Element
addMathInfo element =
  let mathspace = Attr { attrKey = (QName "m" Nothing (Just "xmlns"))
                       , attrVal = "http://schemas.openxmlformats.org/officeDocument/2006/math"
                       }
  in add_attr mathspace element

-- We look through the element to see if it contains an a14:m
-- element. If so, we surround it. This is a bit ugly, but it seems
-- more dependable than looking through shapes for math. Plus this is
-- an xml implementation detail, so it seems to make sense to do it at
-- the xml level.
surroundWithMathAlternate :: Element -> Element
surroundWithMathAlternate element =
  case findElement (QName "m" Nothing (Just "a14")) element of
    Just _ ->
      mknode "mc:AlternateContent"
         [("xmlns:mc", "http://schemas.openxmlformats.org/markup-compatibility/2006")
         ] [ mknode "mc:Choice"
             [ ("xmlns:a14", "http://schemas.microsoft.com/office/drawing/2010/main")
             , ("Requires", "a14")] [ element ]
           ]
    Nothing -> element

paragraphToElement :: PandocMonad m => Paragraph -> P m Element
paragraphToElement par = do
  let
    attrs = [("lvl", show $ pPropLevel $ paraProps par)] ++
            (case pPropMarginLeft (paraProps par) of
               Just px -> [("marL", show $ 12700 * px), ("indent", "0")]
               Nothing -> []
            ) ++
            (case pPropAlign (paraProps par) of
               Just AlgnLeft -> [("algn", "l")]
               Just AlgnRight -> [("algn", "r")]
               Just AlgnCenter -> [("algn", "ctr")]
               Nothing -> []
            )
    props = [] ++
            (case pPropSpaceBefore $ paraProps par of
               Just px -> [mknode "a:spcBef" [] [
                              mknode "a:spcPts" [("val", show $ 100 * px)] ()
                              ]
                          ]
               Nothing -> []
            ) ++
            (case pPropBullet $ paraProps par of
               Just Bullet -> []
               Just (AutoNumbering attrs') ->
                 [mknode "a:buAutoNum" [("type", autoNumberingToType attrs')] ()]
               Nothing -> [mknode "a:buNone" [] ()]
            )
  paras <- mapM paraElemToElement (combineParaElems $ paraElems par)
  return $ mknode "a:p" [] $ [mknode "a:pPr" attrs props] ++ paras

shapeToElement :: PandocMonad m => Element -> Shape -> P m Element
shapeToElement layout (TextBox paras)
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      sp <- getContentShape ns spTree
      elements <- mapM paragraphToElement paras
      let txBody = mknode "p:txBody" [] $
                   [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] ++ elements
          emptySpPr = mknode "p:spPr" [] ()
      return $
        surroundWithMathAlternate $
        replaceNamedChildren ns "p" "txBody" [txBody] $
        replaceNamedChildren ns "p" "spPr" [emptySpPr] $
        sp
-- GraphicFrame and Pic should never reach this.
shapeToElement _ _ = return $ mknode "p:sp" [] ()

shapeToElements :: PandocMonad m => Element -> Shape -> P m [Element]
shapeToElements layout (Pic picProps fp attr alt) = do
  mInfo <- registerMedia fp alt
  case mInfoExt mInfo of
    Just _ -> do
      makePicElements layout picProps mInfo attr alt
    Nothing -> shapeToElements layout $ TextBox [Paragraph def alt]
shapeToElements layout (GraphicFrame tbls cptn) =
  graphicFrameToElements layout tbls cptn
shapeToElements layout shp = do
  element <- shapeToElement layout shp
  return [element]

shapesToElements :: PandocMonad m => Element -> [Shape] -> P m [Element]
shapesToElements layout shps = do
 concat <$> mapM (shapeToElements layout) shps

hardcodedTableMargin :: Integer
hardcodedTableMargin = 36

graphicFrameToElements :: PandocMonad m => Element -> [Graphic] -> [ParaElem] -> P m [Element]
graphicFrameToElements layout tbls caption = do
  -- get the sizing
  master <- getMaster
  (pageWidth, pageHeight) <- asks envPresentationSize
  let ns = elemToNameSpaces layout
  ((x, y), (cx, cytmp)) <- getContentShapeSize ns layout master
                           `catchError`
                           (\_ -> return ((0, 0), (pageWidth, pageHeight)))

  let cy = if (not $ null caption) then cytmp - captionHeight else cytmp

  elements <- mapM graphicToElement tbls
  let graphicFrameElts =
        mknode "p:graphicFrame" [] $
        [ mknode "p:nvGraphicFramePr" [] $
          [ mknode "p:cNvPr" [("id", "6"), ("name", "Content Placeholder 5")] ()
          , mknode "p:cNvGraphicFramePr" [] $
            [mknode "a:graphicFrameLocks" [("noGrp", "1")] ()]
          , mknode "p:nvPr" [] $
            [mknode "p:ph" [("idx", "1")] ()]
          ]
        , mknode "p:xfrm" [] $
          [ mknode "a:off" [("x", show $ 12700 * x), ("y", show $ 12700 * y)] ()
          , mknode "a:ext" [("cx", show $ 12700 * cx), ("cy", show $ 12700 * cy)] ()
          ]
        ] ++ elements

  if (not $ null caption)
    then do capElt <- createCaption ((x, y), (cx, cytmp)) caption
            return [graphicFrameElts, capElt]
    else return [graphicFrameElts]

graphicToElement :: PandocMonad m => Graphic -> P m Element
graphicToElement (Tbl tblPr colWidths hdrCells rows) = do
  let cellToOpenXML paras =
        do elements <- mapM paragraphToElement paras
           let elements' = if null elements
                           then [mknode "a:p" [] [mknode "a:endParaRPr" [] ()]]
                           else elements
           return $
             [mknode "a:txBody" [] $
               ([ mknode "a:bodyPr" [] ()
                , mknode "a:lstStyle" [] ()]
                 ++ elements')]
  headers' <- mapM cellToOpenXML hdrCells
  rows' <- mapM (mapM cellToOpenXML) rows
  let borderProps = mknode "a:tcPr" [] ()
  let emptyCell = [mknode "a:p" [] [mknode "a:pPr" [] ()]]
  let mkcell border contents = mknode "a:tc" []
                            $ (if null contents
                               then emptyCell
                               else contents) ++ [ borderProps | border ]
  let mkrow border cells = mknode "a:tr" [("h", "0")] $ map (mkcell border) cells

  let mkgridcol w = mknode "a:gridCol"
                       [("w", show ((12700 * w) :: Integer))] ()
  let hasHeader = not (all null hdrCells)
  return $ mknode "a:graphic" [] $
    [mknode "a:graphicData" [("uri", "http://schemas.openxmlformats.org/drawingml/2006/table")] $
     [mknode "a:tbl" [] $
      [ mknode "a:tblPr" [ ("firstRow", if tblPrFirstRow tblPr then "1" else "0")
                         , ("bandRow", if tblPrBandRow tblPr then "1" else "0")
                         ] ()
      , mknode "a:tblGrid" [] (if all (==0) colWidths
                               then []
                               else map mkgridcol colWidths)
      ]
      ++ [ mkrow True headers' | hasHeader ] ++ map (mkrow False) rows'
     ]
    ]

getShapeByName :: NameSpaces -> Element -> String -> Maybe Element
getShapeByName ns spTreeElem name
  | isElem ns "p" "spTree" spTreeElem =
  filterChild (\e -> (isElem ns "p" "sp" e) && (shapeHasName ns name e)) spTreeElem
  | otherwise = Nothing

-- getShapeById :: NameSpaces -> Element -> String -> Maybe Element
-- getShapeById ns spTreeElem ident
--   | isElem ns "p" "spTree" spTreeElem =
--   filterChild (\e -> (isElem ns "p" "sp" e) && (shapeHasId ns ident e)) spTreeElem
--   | otherwise = Nothing

nonBodyTextToElement :: PandocMonad m => Element -> String -> [ParaElem] -> P m Element
nonBodyTextToElement layout shapeName paraElements
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld
  , Just sp <- getShapeByName ns spTree shapeName = do
      let hdrPara = Paragraph def paraElements
      element <- paragraphToElement hdrPara
      let txBody = mknode "p:txBody" [] $
                   [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] ++
                   [element]
      return $ replaceNamedChildren ns "p" "txBody" [txBody] sp
  -- XXX: TODO
  | otherwise = return $ mknode "p:sp" [] ()

contentToElement :: PandocMonad m => Element -> [ParaElem] -> [Shape] -> P m Element
contentToElement layout hdrShape shapes
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      element <- nonBodyTextToElement layout "Title 1" hdrShape
      let hdrShapeElements = if null hdrShape
                             then []
                             else [element]
      contentElements <- shapesToElements layout shapes
      return $
        replaceNamedChildren ns "p" "sp"
        (hdrShapeElements ++ contentElements)
        spTree
contentToElement _ _ _ = return $ mknode "p:sp" [] ()

setIdx'' :: NameSpaces -> String -> Content -> Content
setIdx'' _ idx (Elem element) =
  let tag = XMLC.getTag element
      attrs = XMLC.tagAttribs tag
      idxKey = (QName "idx" Nothing Nothing)
      attrs' = Attr idxKey idx : (filter (\a -> attrKey a /= idxKey) attrs)
      tag' = tag {XMLC.tagAttribs = attrs'}
  in Elem $ XMLC.setTag tag' element
setIdx'' _ _ c = c

setIdx' :: NameSpaces -> String -> XMLC.Cursor -> XMLC.Cursor
setIdx' ns idx cur =
  let modifiedCur = XMLC.modifyContent (setIdx'' ns idx) cur
  in
    case XMLC.nextDF modifiedCur of
      Just cur' -> setIdx' ns idx cur'
      Nothing   -> XMLC.root modifiedCur

setIdx :: NameSpaces -> String -> Element -> Element
setIdx ns idx element =
  let cur = XMLC.fromContent (Elem element)
      cur' = setIdx' ns idx cur
  in
    case XMLC.toTree cur' of
      Elem element' -> element'
      _             -> element

twoColumnToElement :: PandocMonad m => Element -> [ParaElem] -> [Shape] -> [Shape] -> P m Element
twoColumnToElement layout hdrShape shapesL shapesR
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      element <- nonBodyTextToElement layout "Title 1" hdrShape
      let hdrShapeElements = if null hdrShape
                             then []
                             else [element]
      contentElementsL <- shapesToElements layout shapesL
      contentElementsR <- shapesToElements layout shapesR
      let contentElementsL' = map (setIdx ns "1") contentElementsL
          contentElementsR' = map (setIdx ns "2") contentElementsR
      return $
        replaceNamedChildren ns "p" "sp"
        (hdrShapeElements ++ contentElementsL' ++ contentElementsR')
        spTree
twoColumnToElement _ _ _ _= return $ mknode "p:sp" [] ()


titleToElement :: PandocMonad m => Element -> [ParaElem] -> P m Element
titleToElement layout titleElems
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      element <- nonBodyTextToElement layout "Title 1" titleElems
      let titleShapeElements = if null titleElems
                               then []
                               else [element]
      return $ replaceNamedChildren ns "p" "sp" titleShapeElements spTree
titleToElement _ _ = return $ mknode "p:sp" [] ()

metadataToElement :: PandocMonad m => Element -> [ParaElem] -> [ParaElem] -> [[ParaElem]] -> [ParaElem] -> P m Element
metadataToElement layout titleElems subtitleElems authorsElems dateElems
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      titleShapeElements <- if null titleElems
                            then return []
                            else sequence [nonBodyTextToElement layout "Title 1" titleElems]
      let combinedAuthorElems = intercalate [Break] authorsElems
          subtitleAndAuthorElems = intercalate [Break, Break] [subtitleElems, combinedAuthorElems]
      subtitleShapeElements <- if null subtitleAndAuthorElems
                               then return []
                               else sequence [nonBodyTextToElement layout "Subtitle 2" subtitleAndAuthorElems]
      dateShapeElements <- if null dateElems
                           then return []
                           else sequence [nonBodyTextToElement layout "Date Placeholder 3" dateElems]
      return $ replaceNamedChildren ns "p" "sp"
        (titleShapeElements ++ subtitleShapeElements ++ dateShapeElements)
        spTree
metadataToElement _ _ _ _ _ = return $ mknode "p:sp" [] ()

slideToElement :: PandocMonad m => Slide -> P m Element
slideToElement s@(ContentSlide hdrElems shapes) = do
  layout <- getLayout s
  spTree <- local (\env -> if null hdrElems
                           then env
                           else env{envSlideHasHeader=True}) $
            contentToElement layout hdrElems shapes
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]
slideToElement s@(TwoColumnSlide hdrElems shapesL shapesR) = do
  layout <- getLayout s
  spTree <- local (\env -> if null hdrElems
                           then env
                           else env{envSlideHasHeader=True}) $
            twoColumnToElement layout hdrElems shapesL shapesR
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]
slideToElement s@(TitleSlide hdrElems) = do
  layout <- getLayout s
  spTree <- titleToElement layout hdrElems
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]
slideToElement s@(MetadataSlide titleElems subtitleElems authorElems dateElems) = do
  layout <- getLayout s
  spTree <- metadataToElement layout titleElems subtitleElems authorElems dateElems
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]

-----------------------------------------------------------------------

slideToFilePath :: Slide -> Int -> FilePath
slideToFilePath _ idNum = "slide" ++ (show $ idNum) ++ ".xml"

slideToSlideId :: Monad m => Slide -> Int -> P m String
slideToSlideId _ idNum = do
  n <- asks envSlideIdOffset
  return $ "rId" ++ (show $ idNum + n)


data Relationship = Relationship { relId :: Int
                                 , relType :: MimeType
                                 , relTarget :: FilePath
                                 } deriving (Show, Eq)

elementToRel :: Element -> Maybe Relationship
elementToRel element
  | elName element == QName "Relationship" (Just "http://schemas.openxmlformats.org/package/2006/relationships") Nothing =
      do rId <- findAttr (QName "Id" Nothing Nothing) element
         numStr <- stripPrefix "rId" rId
         num <- case reads numStr :: [(Int, String)] of
           (n, _) : _ -> Just n
           []         -> Nothing
         type' <- findAttr (QName "Type" Nothing Nothing) element
         target <- findAttr (QName "Target" Nothing Nothing) element
         return $ Relationship num type' target
  | otherwise = Nothing

slideToPresRel :: Monad m => Slide -> Int -> P m Relationship
slideToPresRel slide idNum = do
  n <- asks envSlideIdOffset
  let rId = idNum + n
      fp = "slides/" ++ slideToFilePath slide idNum
  return $ Relationship { relId = rId
                        , relType = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide"
                        , relTarget = fp
                        }

getRels :: PandocMonad m => P m [Relationship]
getRels = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  relsElem <- parseXml refArchive distArchive "ppt/_rels/presentation.xml.rels"
  let globalNS = "http://schemas.openxmlformats.org/package/2006/relationships"
  let relElems = findChildren (QName "Relationship" (Just globalNS) Nothing) relsElem
  return $ mapMaybe elementToRel relElems

presentationToRels :: PandocMonad m => Presentation -> P m [Relationship]
presentationToRels (Presentation slides) = do
  mySlideRels <- mapM (\(s, n) -> slideToPresRel s n) $ zip slides [1..]
  rels <- getRels
  let relsWithoutSlides = filter (\r -> relType r /= "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide") rels
  -- We want to make room for the slides in the id space. The slides
  -- will start at Id2 (since Id1 is for the slide master). There are
  -- two slides in the data file, but that might change in the future,
  -- so we will do this:
  --
  -- 1. We look to see what the minimum relWithoutSlide id (greater than 1) is.
  -- 2. We add the difference between this and the number of slides to
  -- all relWithoutSlide rels (unless they're 1)

  let minRelNotOne = case filter (1<) $ map relId relsWithoutSlides of
        [] -> 0                 -- doesn't matter in this case, since
                                -- there will be nothing to map the
                                -- function over
        l  -> minimum l

      modifyRelNum :: Int -> Int
      modifyRelNum 1 = 1
      modifyRelNum n = n - minRelNotOne + 2 + length slides

      relsWithoutSlides' = map (\r -> r{relId = modifyRelNum $ relId r}) relsWithoutSlides

  return $ mySlideRels ++ relsWithoutSlides'

relToElement :: Relationship -> Element
relToElement rel = mknode "Relationship" [ ("Id", "rId" ++ (show $ relId rel))
                                         , ("Type", relType rel)
                                         , ("Target", relTarget rel) ] ()

relsToElement :: [Relationship] -> Element
relsToElement rels = mknode "Relationships"
                     [("xmlns", "http://schemas.openxmlformats.org/package/2006/relationships")]
                     (map relToElement rels)

presentationToRelsEntry :: PandocMonad m => Presentation -> P m Entry
presentationToRelsEntry pres = do
  rels <- presentationToRels pres
  elemToEntry "ppt/_rels/presentation.xml.rels" $ relsToElement rels

elemToEntry :: PandocMonad m => FilePath -> Element -> P m Entry
elemToEntry fp element = do
  epochtime <- (floor . utcTimeToPOSIXSeconds) <$> asks envUTCTime
  return $ toEntry fp epochtime $ renderXml element

slideToEntry :: PandocMonad m => Slide -> Int -> P m Entry
slideToEntry slide idNum = do
  local (\env -> env{envCurSlideId = idNum}) $ do
    element <- slideToElement slide
    elemToEntry ("ppt/slides/" ++ slideToFilePath slide idNum) element

slideToSlideRelEntry :: PandocMonad m => Slide -> Int -> P m Entry
slideToSlideRelEntry slide idNum = do
  element <- slideToSlideRelElement slide idNum
  elemToEntry ("ppt/slides/_rels/" ++ slideToFilePath slide idNum ++ ".rels") element

linkRelElement :: PandocMonad m => Int -> (URL, String) -> P m (Maybe Element)
linkRelElement idNum (url, _) = do
  anchorMap <- gets stAnchorMap
  case url of
    -- if it's an anchor in the map, we use the slide number for an
    -- internal link.
    '#' : anchor | Just num <- M.lookup anchor anchorMap ->
      return $ Just $
      mknode "Relationship" [ ("Id", "rId" ++ show idNum)
                            , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide")
                            , ("Target", "slide" ++ show num ++ ".xml")
                            ] ()
    -- if it's an anchor not in the map, we return nothing.
    '#' : _ -> return Nothing
    -- Anything else we treat as an external link
    _ ->
      return $ Just $
      mknode "Relationship" [ ("Id", "rId" ++ show idNum)
                            , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink")
                        , ("Target", url)
                        , ("TargetMode", "External")
                        ] ()

linkRelElements :: PandocMonad m => M.Map Int (URL, String) -> P m [Element]
linkRelElements mp = catMaybes <$> mapM (\(n, lnk) -> linkRelElement n lnk) (M.toList mp)

mediaRelElement :: MediaInfo -> Element
mediaRelElement mInfo =
  let ext = case mInfoExt mInfo of
              Just e -> e
              Nothing -> ""
  in
    mknode "Relationship" [ ("Id", "rId" ++ (show $ mInfoLocalId mInfo))
                          , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image")
                          , ("Target", "../media/image" ++ (show $ mInfoGlobalId mInfo) ++ ext)
                          ] ()

slideToSlideRelElement :: PandocMonad m => Slide -> Int -> P m Element
slideToSlideRelElement slide idNum = do
  let target =  case slide of
        (MetadataSlide _ _ _ _) -> "../slideLayouts/slideLayout1.xml"
        (TitleSlide _)        -> "../slideLayouts/slideLayout3.xml"
        (ContentSlide _ _)    -> "../slideLayouts/slideLayout2.xml"
        (TwoColumnSlide _ _ _)    -> "../slideLayouts/slideLayout4.xml"

  linkIds <- gets stLinkIds
  mediaIds <- gets stMediaIds

  linkRels <- case M.lookup idNum linkIds of
                Just mp -> linkRelElements mp
                Nothing -> return []
  let mediaRels = case M.lookup idNum mediaIds of
                   Just mInfos -> map mediaRelElement mInfos
                   Nothing -> []

  return $
    mknode "Relationships"
    [("xmlns", "http://schemas.openxmlformats.org/package/2006/relationships")]
    ([mknode "Relationship" [ ("Id", "rId1")
                           , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout")
                           , ("Target", target)] ()
    ] ++ linkRels ++ mediaRels)

slideToSldIdElement :: PandocMonad m => Slide -> Int -> P m Element
slideToSldIdElement slide idNum = do
  let id' = show $ idNum + 255
  rId <- slideToSlideId slide idNum
  return $ mknode "p:sldId" [("id", id'), ("r:id", rId)] ()

presentationToSldIdLst :: PandocMonad m => Presentation -> P m Element
presentationToSldIdLst (Presentation slides) = do
  ids <- mapM (\(s,n) -> slideToSldIdElement s n) (zip slides [1..])
  return $ mknode "p:sldIdLst" [] ids

presentationToPresentationElement :: PandocMonad m => Presentation -> P m Element
presentationToPresentationElement pres = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  element <- parseXml refArchive distArchive "ppt/presentation.xml"
  sldIdLst <- presentationToSldIdLst pres

  let modifySldIdLst :: Content -> Content
      modifySldIdLst (Elem e) = case elName e of
        (QName "sldIdLst" _ _) -> Elem sldIdLst
        _                      -> Elem e
      modifySldIdLst ct = ct

      newContent = map modifySldIdLst $ elContent element

  return $ element{elContent = newContent}

presentationToPresEntry :: PandocMonad m => Presentation -> P m Entry
presentationToPresEntry pres = presentationToPresentationElement pres >>=
  elemToEntry "ppt/presentation.xml"




defaultContentTypeToElem :: DefaultContentType -> Element
defaultContentTypeToElem dct =
  mknode "Default"
  [("Extension", defContentTypesExt dct),
    ("ContentType", defContentTypesType dct)]
  ()

overrideContentTypeToElem :: OverrideContentType -> Element
overrideContentTypeToElem oct =
  mknode "Override"
  [("PartName", overrideContentTypesPart oct),
    ("ContentType", overrideContentTypesType oct)]
  ()

contentTypesToElement :: ContentTypes -> Element
contentTypesToElement ct =
  let ns = "http://schemas.openxmlformats.org/package/2006/content-types"
  in
    mknode "Types" [("xmlns", ns)] $
    (map defaultContentTypeToElem $ contentTypesDefaults ct) ++
    (map overrideContentTypeToElem $ contentTypesOverrides ct)

data DefaultContentType = DefaultContentType
                           { defContentTypesExt :: String
                           , defContentTypesType:: MimeType
                           }
                         deriving (Show, Eq)

data OverrideContentType = OverrideContentType
                           { overrideContentTypesPart :: FilePath
                           , overrideContentTypesType :: MimeType
                           }
                          deriving (Show, Eq)

data ContentTypes = ContentTypes { contentTypesDefaults :: [DefaultContentType]
                                 , contentTypesOverrides :: [OverrideContentType]
                                 }
                    deriving (Show, Eq)

contentTypesToEntry :: PandocMonad m => ContentTypes -> P m Entry
contentTypesToEntry ct = elemToEntry "[Content_Types].xml" $ contentTypesToElement ct

pathToOverride :: FilePath -> Maybe OverrideContentType
pathToOverride fp = OverrideContentType ("/" ++ fp) <$> (getContentType fp)

mediaFileContentType :: FilePath -> Maybe DefaultContentType
mediaFileContentType fp = case takeExtension fp of
  '.' : ext -> Just $
               DefaultContentType { defContentTypesExt = ext
                                  , defContentTypesType =
                                      case getMimeType fp of
                                        Just mt -> mt
                                        Nothing -> "application/octet-stream"
                                  }
  _ -> Nothing

mediaContentType :: MediaInfo -> Maybe DefaultContentType
mediaContentType mInfo
  | Just ('.' : ext) <- mInfoExt mInfo =
      Just $ DefaultContentType { defContentTypesExt = ext
                                , defContentTypesType =
                                    case mInfoMimeType mInfo of
                                      Just mt -> mt
                                      Nothing -> "application/octet-stream"
                                }
  | otherwise = Nothing

presentationToContentTypes :: PandocMonad m => Presentation -> P m ContentTypes
presentationToContentTypes (Presentation slides) = do
  mediaInfos <- (mconcat . M.elems) <$> gets stMediaIds
  filePaths <- patternsToFilePaths inheritedPatterns
  let mediaFps = filter (match (compile "ppt/media/image*")) filePaths
  let defaults = [ DefaultContentType "xml" "application/xml"
                 , DefaultContentType "rels" "application/vnd.openxmlformats-package.relationships+xml"
                 ]
      mediaDefaults = nub $
                      (mapMaybe mediaContentType $ mediaInfos) ++
                      (mapMaybe mediaFileContentType $ mediaFps)

      inheritedOverrides = mapMaybe pathToOverride filePaths
      presOverride = mapMaybe pathToOverride ["ppt/presentation.xml"]
      slideOverrides =
        mapMaybe
        (\(s, n) ->
           pathToOverride $ "ppt/slides/" ++ slideToFilePath s n)
        (zip slides [1..])
      -- propOverride = mapMaybe pathToOverride ["docProps/core.xml"]
  return $ ContentTypes
    (defaults ++ mediaDefaults)
    (inheritedOverrides ++ presOverride ++ slideOverrides)

presML :: String
presML = "application/vnd.openxmlformats-officedocument.presentationml"

noPresML :: String
noPresML = "application/vnd.openxmlformats-officedocument"

getContentType :: FilePath -> Maybe MimeType
getContentType fp
  | fp == "ppt/presentation.xml" = Just $ presML ++ ".presentation.main+xml"
  | fp == "ppt/presProps.xml" = Just $ presML ++ ".presProps+xml"
  | fp == "ppt/viewProps.xml" = Just $ presML ++ ".viewProps+xml"
  | fp == "ppt/tableStyles.xml" = Just $ presML ++ ".tableStyles+xml"
  | fp == "docProps/core.xml" = Just $ "application/vnd.openxmlformats-package.core-properties+xml"
  | fp == "docProps/app.xml" = Just $ noPresML ++ ".extended-properties+xml"
  | "ppt" : "slideMasters" : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML ++ ".slideMaster+xml"
  | "ppt" : "slides" : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML ++ ".slide+xml"
  | "ppt" : "notesMasters"  : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML ++ ".notesMaster+xml"
  | "ppt" : "notesSlides"  : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML ++ ".notesSlide+xml"
  | "ppt" : "theme" : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ noPresML ++ ".theme+xml"
  | "ppt" : "slideLayouts" : _ : [] <- splitDirectories fp=
      Just $ presML ++ ".slideLayout+xml"
  | otherwise = Nothing

-------------------------------------------------------

combineParaElems' :: Maybe ParaElem -> [ParaElem] -> [ParaElem]
combineParaElems' mbPElem [] = maybeToList mbPElem
combineParaElems' Nothing (pElem : pElems) =
  combineParaElems' (Just pElem) pElems
combineParaElems' (Just pElem') (pElem : pElems)
  | Run rPr' s' <- pElem'
  , Run rPr s <- pElem
  , rPr == rPr' =
    combineParaElems' (Just $ Run rPr' $ s' ++ s) pElems
  | otherwise =
    pElem' : combineParaElems' (Just pElem) pElems

combineParaElems :: [ParaElem] -> [ParaElem]
combineParaElems = combineParaElems' Nothing
