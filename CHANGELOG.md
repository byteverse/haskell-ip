# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 1.7.8 -- 2024-03-01

* Update package metadata.
* Relaxed upper bounds on dependencies.

## 1.7.7 -- 2023-08-24

* Add `isSubsetOf` for `IPv4` and `IPv6` ranges.

## 1.7.6 -- 2022-10-07

* Bump upper bound on `text` to `< 2.1`.
* Add `Hashable` instances for `IP` and `IPv6`.

## 1.7.5 -- 2022-07-28

* Add `boundedBuilderOctetsBE` and `boundedBuilderOctetsLE` to `Net.IPv4`.
* Make doctests work again. Requires `doctest-0.20` or higher.
* Bump upper bound on `attoparsec` to `< 0.15`.
* Bump upper bound on `hashable` to `< 1.5`.
* Derive `Generic` for `Net.IPv6.IPv6`.
* Bump lower bound on `wide-word` to `>= 0.1.1.2`.
* Add compatibility with GHC 9.2.3.

## 1.7.4 -- 2021-12-28

* Add `decodeUtf8Bytes` to `Net.IP`.
* Fix IPv4 octet overflow bug (#74)

## 1.7.3 -- 2021-01-22

* Export `decodeOctets` from `Net.Mac`.
* Add `encodeShort` to `Net.Mac`.

## 1.7.2 -- 2020-05-30
* Fix bug in `doctest` documentation
* Bump upper bound on `aeson`: (< 1.5) -> (< 1.6)

## 1.7.1 -- 2020-01-22
* Deprecate 'decodeBytes' in favor of 'decodeOctets'.
* Add `Bytes`-oriented encode and decode functions to `Net.Mac`:
  `boundedBuilderUtf8`, `decodeUtf8Bytes`, and `parserUtf8Bytes`.
* Add `parserRangeUtf8Bytes` and `parserRangeUtf8BytesLenient` to
  both `Net.IPv4` and `Net.IPv6` modules.

## 1.7.0 -- 2019-11-05
* Add `Data` instances for all types.
* Add `Ix` instances for all address types.
* Add missing `ToJSON`/`FromJSON` instances for `IPv6Range`.
* Remove `Num`, `Integral`, and `Real` instances from `IPv6`.
* Remove `Bits` instance for `IPv4Range`.
* Switch to derived `Bits` instance for `IPv4.
* Remove old spec test for IPv4 Bits laws, instead use
  quickcheck-classes.
* Bump exclusive upper bound on small-bytearray-builder

## 1.6.0 -- 2019-09-30
* Provide decode functions for decoding from `ShortText` and
  from `Bytes`. These two are implemented internally using
  the same function.
* Dependency on `bytesmith` effectively restricts users to
  GHC 8.6 and up. Since GHC 8.8 is about to be released,
  this is deemed an acceptable cost.
* Require cabal version 2.2 so that leading commas are accepted
  in dependencies lists.

## 1.5.1 -- 2019-07-29
* Allow building with primitive-0.7.
* Add more doctests to Net.IP.
* Add to Net.IP: `isIPv4` and `isIPv6`.
* Bump lower bound on primitive from 0.6 to 0.6.4.
* Bump upper bound on hashable from < 1.3. to < 1.4.

## 1.5.0 -- 2019-03-23
* Implement `IPv6` using `wide-word`'s `Word128`. (This is a breaking change.)

## 1.4.2.1 -- 2019-03-18
* Docfix for `Net.IPv4.toList`

## 1.4.2 -- 2019-03-14
* Fix existing `spec` test suite.
* 100% haddock coverage, along with significantly more doctest coverage.
* Add `Net.IPv4.localhost` and `Net.IPv6.localhost`, aliases for `loopback`.

## 1.4.1 -- 2018-08-19
* Add `Enum` and `Bounded` instances for `Mac`.
* Add `NFData` instances for all types.

## 1.4.0 -- 2018-07-18
* Combine `Net.IPv4` and `Net.IPv4.Range` modules.
* Add `IPv6Range`.
* Drop support for older aeson.
* Add `Enum` instance for `IPv6`.

## 1.2.1 -- 2018-05-10
* Added a `Prim` instance for `Mac`.
