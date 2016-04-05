module Test where

import Data.Scientific
import Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Test.QuickCheck
import Servant (Proxy(..), NamedContext)
import Servant.Mock (mock)
import Servant.Server (serve)
import Network.Wai.Handler.Warp (run)

import Types
import Address
import Author
import Publisher
import Book
import API (api)

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Day where
  arbitrary = fromGregorian <$> year <*> month <*> day
    where
      year = elements [1970..2016]
      month = elements [1..12]
      day = elements [1..31]

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> pure 0

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
  arbitrary = Address <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Gender where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Author where
  arbitrary = Author <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> age <*> arbitrary
    where
      age = arbitrary `suchThat` ((&&) <$> (0<) <*> (<120))

instance Arbitrary CompanyType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary Publisher where
  arbitrary = Publisher <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AddressId where
  arbitrary = AddressId <$> arbitrary `suchThat` (>0)

instance Arbitrary AuthorId where
  arbitrary = AuthorId <$> arbitrary `suchThat` (>0)

instance Arbitrary PublisherId where
  arbitrary = PublisherId <$> arbitrary `suchThat` (>0)

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

instance Arbitrary Book where
  arbitrary = Book <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> authors <*> arbitrary
    where
      authors = resize 3 $ listOf1 arbitrary

main :: IO ()
main = run 8081 $ serve api (mock api (Proxy :: Proxy '[NamedContext "test" '[]]))
