-- | Byte-exact parsing and rendering of git objects.
--
-- The invariant everything relies on: for any object git gives us,
-- @render <$> parse body == Just body@. If that holds, rewriting with no
-- changes reproduces every hash exactly.
module Git.Object
  ( parseCommit
  , renderCommit
  , parseTree
  , renderTree
  , parseSignature
  , renderSignature
  , objectHeader
  , hashObject
  , checkRoundTrip
  ) where

import Control.Monad (guard)
import Crypto.Hash.SHA1 qualified as SHA1
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Git.Types

-- | What git actually hashes: @"<type> <size>\\0"@ followed by the body.
objectHeader :: ObjectType -> ByteString -> ByteString
objectHeader t body = objectTypeName t <> " " <> BC.pack (show (BS.length body)) <> "\0"

hashObject :: ObjectType -> ByteString -> Oid
hashObject t body =
  fromMaybe (error "hashObject: SHA-1 digest was not 20 bytes") $
    oidFromRaw (SHA1.hash (objectHeader t body <> body))

-- | @Nothing@ when the object parses, renders back to identical bytes and
-- hashes to its id; otherwise a description of the first problem.
checkRoundTrip :: Oid -> ObjectType -> ByteString -> Maybe String
checkRoundTrip oid t body
  | hashObject t body /= oid = Just "hash of body does not match object id"
  | otherwise = case t of
      ObjCommit -> check "commit" (renderCommit <$> parseCommit body)
      ObjTree   -> check "tree" (renderTree <$> parseTree body)
      _         -> Nothing
  where
    check what = \case
      Nothing -> Just (what <> " failed to parse")
      Just rendered
        | rendered /= body -> Just (what <> " rendered differently")
        | otherwise -> Nothing

-- Signatures -----------------------------------------------------------------

-- | @Name <email> 1700000000 +0100@. Rejects anything that would not render
-- back identically (e.g. a timestamp with leading zeros).
parseSignature :: ByteString -> Maybe Signature
parseSignature s = do
  let (nameSp, r1) = BC.break (== '<') s
  name <- BS.stripSuffix " " nameSp
  r2 <- BS.stripPrefix "<" r1
  let (email, r3) = BC.break (== '>') r2
  r4 <- BS.stripPrefix "> " r3
  let (timeText, r5) = BC.break (== ' ') r4
  tz <- BS.stripPrefix " " r5
  (n, rest) <- BC.readInteger timeText
  guard (BS.null rest)
  let time = fromInteger n
  guard (BC.pack (show time) == timeText)
  pure (Signature name email time tz)

renderSignature :: Signature -> ByteString
renderSignature (Signature name email time tz) =
  name <> " <" <> email <> "> " <> BC.pack (show time) <> " " <> tz

-- Commits --------------------------------------------------------------------

parseCommit :: ByteString -> Maybe Commit
parseCommit body = do
  let (headerBlock, rest) = BS.breakSubstring "\n\n" body
  message <- BS.stripPrefix "\n\n" rest
  headers <- parseHeaders headerBlock
  case headers of
    ("tree", t) : more -> do
      tree <- oidFromHex t
      let (parentHeaders, more') = span ((== "parent") . fst) more
      parents <- traverse (oidFromHex . snd) parentHeaders
      case more' of
        ("author", a) : ("committer", c) : extras ->
          Commit tree parents
            <$> parseSignature a
            <*> parseSignature c
            <*> pure extras
            <*> pure message
        _ -> Nothing
    _ -> Nothing

renderCommit :: Commit -> ByteString
renderCommit c = foldMap renderHeader headers <> "\n" <> cMessage c
  where
    headers =
      ("tree", oidToHex (cTree c))
        : map (("parent",) . oidToHex) (cParents c)
        <> [ ("author", renderSignature (cAuthor c))
           , ("committer", renderSignature (cCommitter c))
           ]
        <> cExtra c

-- | Header lines are @key value@; a line starting with a space continues the
-- previous value (this is how multi-line gpgsig blocks are stored).
parseHeaders :: ByteString -> Maybe [(ByteString, ByteString)]
parseHeaders = fmap reverse . foldl step (Just []) . BC.split '\n'
  where
    step acc line = do
      hs <- acc
      case BS.stripPrefix " " line of
        Just continuation -> case hs of
          (k, v) : older -> Just ((k, v <> "\n" <> continuation) : older)
          [] -> Nothing
        Nothing -> do
          let (k, r) = BC.break (== ' ') line
          v <- BS.stripPrefix " " r
          guard (not (BS.null k))
          Just ((k, v) : hs)

renderHeader :: (ByteString, ByteString) -> ByteString
renderHeader (k, v) = k <> " " <> BS.intercalate "\n " (BC.split '\n' v) <> "\n"

-- Trees ----------------------------------------------------------------------

-- | Entries are @<mode> <name>\\0<20 raw bytes>@, back to back, no separators.
parseTree :: ByteString -> Maybe Tree
parseTree bs
  | BS.null bs = Just []
  | otherwise = do
      let (mode, r1) = BC.break (== ' ') bs
      r2 <- BS.stripPrefix " " r1
      let (name, r3) = BS.break (== 0) r2
      r4 <- BS.stripPrefix "\0" r3
      let (raw, r5) = BS.splitAt 20 r4
      oid <- oidFromRaw raw
      (TreeEntry mode name oid :) <$> parseTree r5

renderTree :: Tree -> ByteString
renderTree = foldMap $ \e -> teMode e <> " " <> teName e <> "\0" <> oidToRaw (teOid e)
