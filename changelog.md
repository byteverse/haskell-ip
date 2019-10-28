# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [1.7.0] - 2019-10-28
- Add `Data` instances for all types.
- Add `Ix` instances for all address types.
- Add missing `ToJSON`/`FromJSON` instances for `IPv6Range`.
- Remove `Num`, `Integral`, and `Real` instances from `IPv6`.

## [1.6.0] - 2019-09-30
- Provide decode functions for decoding from `ShortText` and
  from `Bytes`. These two are implemented internally using
  the same function.
- Dependency on `bytesmith` effectively restricts users to
  GHC 8.6 and up. Since GHC 8.8 is about to be released,
  this is deemed an acceptable cost.
- Require cabal version 2.2 so that leading commas are accepted
  in dependencies lists.

## [1.5.1] - 2019-07-29
- Allow building with primitive-0.7.
- Add more doctests to Net.IP.
- Add to Net.IP: `isIPv4` and `isIPv6`.
- Bump lower bound on primitive from 0.6 to 0.6.4.
- Bump upper bound on hashable from < 1.3. to < 1.4.

## [1.5.0] - 2019-03-23
- Implement `IPv6` using `wide-word`'s `Word128`. (This is a breaking change.)

## [1.4.2.1] - 2019-03-18
- Docfix for `Net.IPv4.toList`

## [1.4.2] - 2019-03-14
- Fix existing `spec` test suite.
- 100% haddock coverage, along with significantly more doctest coverage.
- Add `Net.IPv4.localhost` and `Net.IPv6.localhost`, aliases for `loopback`.

## [1.4.1] - 2018-08-19
- Add `Enum` and `Bounded` instances for `Mac`.
- Add `NFData` instances for all types.

## [1.4.0] - 2018-07-18
- Combine `Net.IPv4` and `Net.IPv4.Range` modules.
- Add `IPv6Range`.
- Drop support for older aeson.
- Add `Enum` instance for `IPv6`.

## [1.2.1] - 2018-05-10
- Added a `Prim` instance for `Mac`.
