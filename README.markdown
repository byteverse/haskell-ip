# Instructions

Look at the [haddocks](http://hackage.haskell.org/package/ip) for this 
package to learn how to use it.

# Contributing

Most contributions are welcome. Improvements are needed the most
in the encoding/decoding of Text/ByteString. There is a 
process for this so that old implementations can continue to be compared
against. For improved implementations create a new module 
in the `tests` directory. The name should be the concatenation of three things:

1. A data type
2. Either the word Text or ByteString
3. A number

Some examples for names are `IPv4Text6` or `MacByteString2`. This module
should export either a function named `encode` or a function named `decode`.
Then, in `test/Bench.hs`, add the new implementation. Run `stack bench`
to see how it compares with the others.

After doing this, PR those changes and mention that a better
implementation than what is currently being used has been added.
The new implementation will be added to the actual library
if it is better.

# Other

If this readme is confusing in any way, please open up an issue.

