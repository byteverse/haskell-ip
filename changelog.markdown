# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

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
