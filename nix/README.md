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
