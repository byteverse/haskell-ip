import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Net/IPv4.hs"
  , "src/Net/IPv6.hs"
  , "src/Net/IPv4/Range.hs"
  , "src/Data/Word/Synthetic/Word12.hs"
  , "src/Data/Text/Builder/Common/Internal.hs"
  , "src/Data/Text/Builder/Fixed.hs"
  , "src/Data/ByteString/Builder/Fixed.hs"
  , "src/Net/Mac.hs"
  ]
