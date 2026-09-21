-- | Human-readable views of objects, shared by the CLI and the TUI.
module Git.Pretty
  ( logEntry
  , showPretty
  , prettyCommit
  , showRaw
  , escape
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Git.Date (formatSignatureDate)
import Git.Object
import Git.Types
import Numeric (showHex)

-- | One commit for @log@: a headline, plus a second line when the committer
-- or commit date differs from the author's.
logEntry :: Oid -> Commit -> ByteString
logEntry oid c = BS.concat [headline, committedLine]
  where
    a = cAuthor c
    k = cCommitter c
    mark = if length (cParents c) > 1 then " M " else "   "
    subject = BC.takeWhile (/= '\n') (cMessage c)
    headline = shortHex oid <> mark <> formatSignatureDate a <> "  " <> sigName a <> "  " <> subject <> "\n"
    sameWho = (sigName a, sigEmail a) == (sigName k, sigEmail k)
    sameWhen = (sigTime a, sigTz a) == (sigTime k, sigTz k)
    committedLine
      | sameWho && sameWhen = ""
      | otherwise =
          BC.replicate 10 ' ' <> "committed " <> formatSignatureDate k
            <> (if sameWho then "" else "  by " <> sigName k) <> "\n"

showPretty :: Oid -> ObjectType -> ByteString -> ByteString
showPretty oid t body = case t of
  ObjCommit | Just c <- parseCommit body -> prettyCommit oid c
  ObjTree | Just tree <- parseTree body -> foldMap prettyEntry tree
  _ -> body

prettyCommit :: Oid -> Commit -> ByteString
prettyCommit oid c =
  BS.concat $
    [field "commit" (oidToHex oid), field "tree" (oidToHex (cTree c))]
      <> map (field "parent" . oidToHex) (cParents c)
      <> [field "author" (sig (cAuthor c)), field "committer" (sig (cCommitter c))]
      <> [field k (summarise v) | (k, v) <- cExtra c]
      <> ["\n"]
      <> map (\l -> "    " <> l <> "\n") (BC.lines (cMessage c))
  where
    field k v = k <> BC.replicate (max 1 (10 - BS.length k)) ' ' <> v <> "\n"
    sig s = sigName s <> " <" <> sigEmail s <> ">  " <> formatSignatureDate s <> "  (unix " <> BC.pack (show (sigTime s)) <> ")"
    summarise v = case BC.lines v of
      [l] -> l
      ls -> "(" <> BC.pack (show (length ls)) <> " lines)"

prettyEntry :: TreeEntry -> ByteString
prettyEntry e = teMode e <> " " <> kind <> " " <> oidToHex (teOid e) <> "\t" <> teName e <> "\n"
  where
    kind = case teMode e of
      m | m `elem` ["40000", "040000"] -> "tree  "
      "160000" -> "commit"
      _ -> "blob  "

-- | The exact bytes git stores and hashes, and a recomputation of the hash.
showRaw :: Oid -> ObjectType -> ByteString -> ByteString
showRaw oid t body =
  BS.concat
    [ "type:    ", objectTypeName t, "\n"
    , "size:    ", BC.pack (show (BS.length body)), " bytes\n"
    , "hashed:  sha1(\"", escape (objectHeader t body), "\" <> body)\n"
    , "result:  ", oidToHex computed, verdict, "\n"
    , "---\n"
    , rawBody
    ]
  where
    computed = hashObject t body
    verdict
      | computed == oid = "  (matches git)"
      | otherwise = "  (MISMATCH: git says " <> oidToHex oid <> ")"
    rawBody = case t of
      ObjTree | Just tree <- parseTree body ->
        "(one entry per line for readability; the real bytes have no newlines)\n"
          <> foldMap (\e -> escape (teMode e <> " " <> teName e <> "\0") <> escape (oidToRaw (teOid e)) <> "\n") tree
      _ -> body <> (if "\n" `BS.isSuffixOf` body then "" else "\n")

-- | Printable ASCII and newlines as-is; everything else as @\\0@ or @\\xHH@.
escape :: ByteString -> ByteString
escape = BS.concatMap $ \w -> case w of
  0 -> "\\0"
  92 -> "\\\\"
  10 -> "\n"
  _ | w >= 0x20 && w < 0x7f -> BS.singleton w
    | otherwise -> "\\x" <> BC.pack (pad (showHex w ""))
  where
    pad s = replicate (2 - length s) '0' <> s
