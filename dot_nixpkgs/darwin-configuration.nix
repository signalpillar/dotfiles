{ config, pkgs ? import (builtins.fetchTarball {
     name = "nixos-22.11-master";
     url = "https://github.com/NixOS/nixpkgs/archive/15b75800dce80225b44f067c9012b09de37dfad2.tar.gz";
     # Hash obtained using `nix-prefetch-url --unpack <url>`
     sha256 = "0xmza136qf0hssh2a4dq62w7w1xs6rdfxs314pqxqjvvqibf1qb2";
   }) {}, lib, ... }:

let
  # Using of the latest packages
  # edge = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") { };
  # Then in system dependencies:
  # (with edge; [
  #   # go
  #   go_1_17
  #   # placeholder
  #   # graalvm8-ce
  #   # clang_12
  # ])

  pngpaste = pkgs.stdenv.mkDerivation {
    name = "pngpaste_0.2.3";
    version = "0.2.3";
    outputs = [ "out" ];

    src = pkgs.fetchFromGitHub {
      owner = "jcsalterego";
      repo = "pngpaste";
      rev = "67c39829fedb97397b691617f10a68af75cf0867";
      sha256 = "089rqjk7khphs011hz3f355c7z6rjd4ydb4qfygmb4x54z2s7xms";
    };

    buildInputs = [
      pkgs.clang10Stdenv
      pkgs.darwin.apple_sdk.frameworks.Foundation
      pkgs.darwin.apple_sdk.frameworks.Cocoa
    ];

    buildPhase = "make all";

    installPhase = ''
      mkdir -p $out/bin
      cp pngpaste $out/bin/
    '';
  };

in {
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget

  # nixpkgs.overlays = [
  #    (import (builtins.fetchTarball {
  #      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  #      sha256 = "1268rw5c1vzl71pd4cmc8izig01rxw6kvmlpl3cyz3x1cf4wc115";
  #    }))
  # ];

  environment.systemPackages = with pkgs;
    [
      terraform
      vault
      (import (fetchTarball https://github.com/cachix/devenv/archive/refs/tags/v0.5.tar.gz))
      lorri
      babashka

      pngpaste
      aspell
      aspellDicts.en
      aspellDicts.en-computers

      fzf
      entr

      # like jq or yq but for HTML
      pup
      jq
      yq
      # [johnkerl/miller: Miller is like awk, sed, cut, join, and sort for name-indexed data such as CSV, TSV, and tabular JSON](https://github.com/johnkerl/miller)
      miller

      rlwrap

      tldr  # community-driven man pages

      bat   # clone of cat
      cloc

      dive  # tool to explore each layer of the docker image
      ctop  # htop for docker
      kubectl

      ripgrep
      exa
      figlet # show banners

      global

      # file manager
      mc

      readline

      graphviz

      git
      delta

      chezmoi
      fortune
      direnv
      coreutils
      ffmpeg
      gdb
      tree
      youtube-dl

      watch
      curl
      wget

      # 1.16.4
      go

      mtr
      iftop
      wrk
      pstree

      htop

      nodejs

      pandoc

      _1password

      ncdu
      tmux
      tmuxp
      just
      # terminal-notifier # https://github.com/julienXX/terminal-notifier
      # synergy
      unrar
      # (vscode-with-extensions.override (vscodeOptions // { vscode = pkgs.vscodium; }))

      # Makes copying and pasting in Terminal.app work again on MacOS Sierra
      # https://github.com/tmux/tmux/issues/543#issuecomment-248980734
      # https://github.com/tmux/tmux/issues/543
      reattach-to-user-namespace
      neovim

      # Editors
     ((emacsPackagesFor emacs29).emacsWithPackages (epkgs: [
      epkgs.vterm
     ]))

      # Terms
      # kitty
      oh-my-zsh

      # Java 11
      adoptopenjdk-bin

      (python39.withPackages(ps: with ps; [
          pip
          black
          isort
          tox
      ]))
  ];

  services = {
    # Auto upgrade nix package and the daemon service.
    # DO NOT ENABLE DAEMON
    nix-daemon.enable = false;
    lorri.enable = true;
  };

  programs.man.enable = true;
  # so we can install something like unrar that is free but not OSS
  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.allowUnsupportedSystem = false;

  environment.variables = {
    EDITOR = "nvim";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    # source: http://itchyknowsdevs.me/blog/developing-golang-in-nixos/
    GOROOT = [ "${pkgs.go.out}/share/go" ];

    # editor
    LS_COLORS = "ow=01;90:di=01;90:ln=04;90";
    ALTERNATE_EDITOR = "";
    # https://github.com/NixOS/nixpkgs/issues/4521#issuecomment-59080831
    ASPELL_CONF = "dict-dir ${pkgs.aspellDicts.en.out}/lib/aspell";
  };

  environment.systemPath = [
    "$HOME/bin"
    "$HOME/.rd/bin"
    "$HOME/.npm/bin"
    "$HOME/mutable_node_modules/bin"
  ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # # nix.package = pkgs.nix;
  # nix.gc = {
  #   automatic = true;
  #   options = "--delete-older-than 30d";
  # };

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableFzfCompletion = true;
    enableSyntaxHighlighting = true;
    # https://github.com/ben-z/dotfiles/blob/master/nixpkgs/darwin-configuration.nix
    interactiveShellInit = ''
      # Turn on when measuring plugin performance
      # zmodload zsh/zprof
      HISTSIZE=10000
      SAVEHIST=10000
      setopt autocd # auto cd when only path is entered
      setopt nomatch # throw an error on glob matching nothing
    '';
  };

  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  environment.shellAliases = {
    # Use emacsclient to open files in current emacs instance server
    ec = "emacsclient -nw";
    ecw = "emacsclient -cn";

    nixre = "darwin-rebuild switch";
    nixgc = "nix-collect-garbage -d";

    ll = "ls -althF";
    la = "ls -A";
    l = "ls -CF";
    tmux = "tmux -2";
    ddc = "docker-compose";
    vim = "nvim";
  };

  system.defaults = {
    dock.autohide = true;
    dock.orientation = "left";
    dock.showhidden = true;
    dock.mru-spaces = false;

    finder.AppleShowAllExtensions = true;
    finder._FXShowPosixPathInTitle = true;
    finder.QuitMenuItem = true;
    # finder.FXEnableExtensionChangeWarning = false;

    # Increase window resize animation speed
    NSGlobalDomain.NSWindowResizeTime = 0.001;

    ## Keyboard
    # Enable full keyboard access for all controls (e.g. enable Tab in modal dialogs)
    NSGlobalDomain.AppleKeyboardUIMode = 3;
    NSGlobalDomain.InitialKeyRepeat = 15;
    NSGlobalDomain.KeyRepeat = 1;

    ## Trackpad/Mouse
    NSGlobalDomain.AppleShowScrollBars = "WhenScrolling";
    NSGlobalDomain."com.apple.mouse.tapBehavior" = 1; # tap to click
    NSGlobalDomain."com.apple.trackpad.enableSecondaryClick" = true;
    trackpad.TrackpadThreeFingerDrag = true;

    # Disable shadow in screenshots
    screencapture.disable-shadow = true;

    NSGlobalDomain.AppleMeasurementUnits = "Centimeters";
    NSGlobalDomain.AppleMetricUnits = 1;
    NSGlobalDomain.AppleTemperatureUnit = "Celsius";
  };

  fonts = {
     fontDir.enable = false;
     fonts = with pkgs; [
        (nerdfonts.override { fonts = [ "JetBrainsMono" "DroidSansMono" ]; })
        comic-mono
        iosevka
        iosevka-comfy.comfy-wide
        dejavu_fonts
        ibm-plex
        inconsolata
        proggyfonts
        emacs-all-the-icons-fonts
        go-font
        jetbrains-mono
     ];
   };
}

# Inspiration:
# - https://github.com/ben-z/dotfiles/blob/master/nixpkgs/darwin-configuration.nix

# Interesting list of apps in the Brewfile
# https://github.com/biosan/dotfiles/blob/master/config/macos/Brewfile
