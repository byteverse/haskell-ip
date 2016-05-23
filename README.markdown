# Instructions

You should look at the [haddocks](http://hackage.haskell.org/package/ip) for this 
package to learn how to use it.

# Contributing

Most contributions would be welcome. One place where I am very welcoming of 
improvements is in the encoding/decoding of Text/ByteString. I have a little 
process for this so that old implementations can continue to be compared
against. If you have an improved implementation to offer, create a new module 
in the `tests` directory. The name should be the concatenation of three things:

1. A data type
2. Either the word Text or ByteString
3. A number

So, you might create a file named `IPv4Text6` or `MacByteString2`. This module
should export either a function named `encode` or a function named `decode`.
Then, in `test/Bench.hs`, add your implementation. You can run `stack bench`
to see how it compares with the others.

After doing this, just PR those changes and mention that you've added a better
implementation that what is currently being used. I'll copy the implementation
into the actual library if it's better.

# Other

If this readme is confusing in any way, please open up an issue.

