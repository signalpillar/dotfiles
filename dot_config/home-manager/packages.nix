{ pkgs, ... }:
let
  ftn = pkgs.fortune.overrideAttrs (oldAttrs: {
    postInstall = ''
      cp $out/bin/fortune $out/bin/ftn
    '';
  });
in {
  home.packages = [
    pkgs.bashInteractive
    pkgs.bat  # alt cat
    pkgs.comma
    pkgs.coreutils
    pkgs.gnugrep
    pkgs.gnused
    pkgs.htop
    pkgs.jq
    pkgs.libiconv
    pkgs.nix-bash-completions
    pkgs.nixfmt
    pkgs.nodePackages.prettier
    pkgs.nodePackages.typescript
    pkgs.nodejs
    pkgs.ripgrep
    # https://shopify.github.io/shadowenv/
    pkgs.shadowenv
    pkgs.tree
    pkgs.watch
    pkgs.wget
    ftn
    pkgs.jq
    pkgs.dysk  # alt df
    pkgs.du-dust # alt du
    pkgs.typos
    # https://github.com/sharkdp/hyperfine
    # https://github.com/gitleaks/gitleaks

    # https://github.com/wimpysworld/nix-config/blob/main/home-manager/default.nix
    # asciicam # Terminal webcam
    # asciinema-agg # Convert asciinema to .gif
    # asciinema # Terminal recorder
    pkgs.bmon # Modern Unix `iftop`
    pkgs.bandwhich # Modern Unix `iftop`
    pkgs.cpufetch # Terminal CPU info
    pkgs.croc # Terminal file transfer
    pkgs.entr # Modern Unix `watch`
    pkgs.fastfetch # Modern Unix system info
    pkgs.mtr # Modern Unix `traceroute`
    pkgs.onefetch # Terminal git project info
    pkgs.tldr # Modern Unix `man`
    pkgs.unzip # Terminal ZIP extractor
    # upterm # Terminal sharing
    pkgs.yq-go # Terminal `jq` for YAML
  ];
}
