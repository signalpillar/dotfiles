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
    pkgs.bat
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
  ];
}
