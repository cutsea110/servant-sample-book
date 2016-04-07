{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Scientific
import Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Calendar (Day, fromGregorian)
import Test.QuickCheck
import Test.QuickCheck.Instances
import Servant (Proxy(..), NamedContext)
import Servant.Mock (mock)
import Servant.Server (serve)
import Network.Wai.Handler.Warp (run)

import Types
import Address
import Author
import AuthorInfo
import Publisher
import PublisherInfo
import Book
import API (api)

instance Arbitrary Prefecture where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Postcode where
  arbitrary = Postcode <$> (append <$> code3 <*> code4)
    where
      append x y = pack $ x ++ "-" ++ y
      n = elements ['0'..'9']
      code3 = vectorOf 3 n
      code4 = vectorOf 4 n
      
instance Arbitrary Tel where
  arbitrary = Tel <$> (append <$> code3 <*> code4 <*> code4)
    where
      append x y z = pack $ x ++ "-" ++ y ++ "-" ++ z
      n = elements ['0'..'9']
      code3 = (:) <$> pure '0' <*> vectorOf 2 n
      code4 = vectorOf 4 n

instance Arbitrary Fax where
  arbitrary = Fax <$> (append <$> code3 <*> code4 <*> code4)
    where
      append x y z = pack $ x ++ "-" ++ y ++ "-" ++ z
      n = elements ['0'..'9']
      code3 = (:) <$> pure '0' <*> vectorOf 2 n
      code4 = vectorOf 4 n

instance Arbitrary Emailaddress where
  arbitrary = Emailaddress <$> (append <$> term <*> domain)
    where
      append x y = pack $ x ++ "@" ++ y
      h = elements (['a'..'z']++['A'..'Z'])
      t = resize 7 $ listOf1 $ elements (['0'..'9']++['a'..'z']++['.','-','_'])
      d = elements [".com",".co.jp",".org",".or.jp",".net",".ne.jp",".ac.jp"]
      term = (:) <$> h <*> t
      domain = (++) <$> term <*> d

instance Arbitrary Address where
  arbitrary = Address <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Gender where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Author where
  arbitrary = Author <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> age <*> arbitrary <*> arbitrary <*> arbitrary
    where
      age = arbitrary `suchThat` ((&&) <$> (0<) <*> (<120))

instance Arbitrary CompanyType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary Publisher where
  arbitrary = Publisher <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AddressId where
  arbitrary = AddressId <$> arbitrary `suchThat` (>0)
instance {-# OVERLAPPING #-} Arbitrary (Maybe AddressId) where
  arbitrary = Just <$> arbitrary

instance Arbitrary AuthorId where
  arbitrary = AuthorId <$> arbitrary `suchThat` (>0)
instance {-# OVERLAPPING #-} Arbitrary (Maybe AuthorId) where
  arbitrary = Just <$> arbitrary

instance Arbitrary PublisherId where
  arbitrary = PublisherId <$> arbitrary `suchThat` (>0)
instance {-# OVERLAPPING #-} Arbitrary (Maybe PublisherId) where
  arbitrary = Just <$> arbitrary

instance Arbitrary BookId where
  arbitrary = BookId <$> arbitrary `suchThat` (>0)
instance {-# OVERLAPPING #-} Arbitrary (Maybe BookId) where
  arbitrary = Just <$> arbitrary

instance Arbitrary ISBN where
  arbitrary = ISBN <$> (append <$> code3 <*> code1 <*> code2 <*> code6 <*> code1)
    where
      append a b c d e = pack $ "ISBN"++a++"-"++b++"-"++c++"-"++d++"-"++e
      n = elements ['0'..'9']
      code1 = vectorOf 1 n
      code2 = vectorOf 2 n
      code3 = vectorOf 3 n
      code6 = vectorOf 6 n

instance Arbitrary Category where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary AuthorInfo where
  arbitrary = AuthorInfo <$> arbitrary <*> arbitrary

instance Arbitrary PublisherInfo where
  arbitrary = PublisherInfo <$> arbitrary <*> arbitrary

instance Arbitrary Book where
  arbitrary = Book <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> authors <*> arbitrary <*> arbitrary <*> arbitrary
    where
      authors = resize 3 $ listOf1 arbitrary

instance Arbitrary BookList where
  arbitrary = BookList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = run 8081 $ serve api (mock api Proxy)
