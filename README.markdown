[![Hackage](https://img.shields.io/hackage/v/ip.svg)](https://hackage.haskell.org/package/ip)

# Instructions

Look at the [haddocks](http://hackage.haskell.org/package/ip) for this 
package to learn how to use it.

# Contributing

Most contributions are welcome, especially performance improvements in encoding and decoding of Text/ByteString.
Please make sure to follow naming conventions followed in the modules.

## Cached dependencies
Nix users can have all dependencies of `ip` cached when hacking on this project by using `cachix`:

```sh
# install nix
$ bash <(curl https://nixos.org/nix/install)

# install cachix client
$ nix-env -iA cachix -f https://cachix.org/api/v1/install

# start using the binary cache
$ cachix use layer-3-cachix
```

Nix commands will use the cache:

```sh
$ nix-build
copying path '/nix/store/n1gwpmvmcgsbnr0a8ncflhvc59db775h-myproject-1.0.0' from 'https://layer-3-cachix.cachix.org'
```
