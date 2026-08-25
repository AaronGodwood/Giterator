module ObjectSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Gen ()
import Git.Object
import Git.Types
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

-- Shape of a real signed commit: the gpgsig header spans lines,
-- including a continuation line that is just a single space.
signedCommit :: ByteString
signedCommit =
  BC.unlines
    [ "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904"
    , "parent 1111111111111111111111111111111111111111"
    , "author A U Thor <a@example.com> 1700000000 +0100"
    , "committer C O Mitter <c@example.com> 1700003600 -0500"
    , "gpgsig -----BEGIN PGP SIGNATURE-----"
    , " "
    , " iQEzBAABCAAdFiEE"
    , " -----END PGP SIGNATURE-----"
    , ""
    , "Subject line"
    , ""
    , "Body text."
    ]

spec :: Spec
spec = do
  describe "hashObject" $
    it "matches git's hash for the empty tree" $
      oidToHex (hashObject ObjTree "") `shouldBe` "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

  describe "signatures" $ do
    prop "round-trip" $ \s -> parseSignature (renderSignature s) === Just s
    it "reject non-canonical timestamps" $
      parseSignature "A <a@b> 0017 +0000" `shouldBe` Nothing

  describe "commits" $ do
    prop "round-trip" $ \c -> parseCommit (renderCommit c) === Just c
    it "keep multi-line headers byte-exact" $
      renderCommit <$> parseCommit signedCommit `shouldBe` Just signedCommit
    it "expose the joined gpgsig value" $
      (lookup "gpgsig" . cExtra =<< parseCommit signedCommit)
        `shouldBe` Just "-----BEGIN PGP SIGNATURE-----\n\niQEzBAABCAAdFiEE\n-----END PGP SIGNATURE-----"
    it "read author and committer separately" $
      fmap (sigTz . cCommitter) (parseCommit signedCommit) `shouldBe` Just "-0500"

  describe "trees" $
    prop "round-trip" $ \t -> parseTree (renderTree t) === Just t
