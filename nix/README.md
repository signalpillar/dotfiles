# Bootstrap without NixOS

Make sure you have nix installed on your system and then::

```bash
% mkdir -p ~/.nixpkgs/
% cd ~/.nixpkgs
~/.nixpkgs % git clone https://github.com/signalpillar/dotfiles
~/.nixpkgs % ln -s `pwd`/dotfiles/nix/nixpkgs/config.nix config.nix
```

# Install the packages

```bash
nix-env -iA nixpkgs.myPackages
```

# Resources

- [Inspiration](https://github.com/garbas/dotfiles)

# Building the python package

Enter the shell with the build dependencies so we can build the virtual
environment.

```bash
nix-shell \
  -p pkgconfig \
  -p openssl \
  -p libffi \
  -p python36Packages.cryptography \
  -p postgresql \
  -p graphviz
```

`gcc` and some other tools are installed globally.

Snippet from tox file that customises install command.

```
[testenv]
...
install_command = pip install -vv --user --extra-index-url={env:EXTRA_INDEX_URL:} {opts} {packages}
```
