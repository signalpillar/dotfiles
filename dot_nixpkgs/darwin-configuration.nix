# darwin-rebuild switch -I darwin-config=$HOME/.nixpkgs/darwin-configuration.nix
{ config,
  pkgs ? import (builtins.fetchTarball {
    name = "nixpkgs-24.05-darwin-stable";
    # https://github.com/NixOS/nixpkgs/commit/37df9bcf93431c7f9f9358aec2d7ed0a52d7ba1d
    url = "https://github.com/NixOS/nixpkgs/archive/37df9bcf93431c7f9f9358aec2d7ed0a52d7ba1d.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1c404j7p2qapccyik0a40rx9n98s27vbx50r0i8d0m3zas6gd25f";
  }) {},
  lib, ... }:

let
  unstable = import (builtins.fetchTarball {
    name = "nixpkgs-stable";
    url = "https://github.com/NixOS/nixpkgs/archive/30439d93eb8b19861ccbe3e581abf97bdc91b093.tar.gz";
    sha256 = "1fa67745sf5f9mdz4slkk814dzn93bi72k25wafi21w74mw43id3";
  }) {};
in {
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # https://github.com/nix-community/emacs-overlay/blob/master/overlays/emacs.nix#L94-L106
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

  environment.systemPackages = with pkgs;
    [
      mpv

      awscli2
      starship
      terraform-ls       # official terraform language server
      hadolint           # Dockerfile linter
      # mplayer

      terraform
      # vault
      # devbox # now installed through sh
      # (import (fetchTarball {
      #   url = https://github.com/cachix/devenv/archive/refs/tags/v1.0.1.tar.gz;
      #   sha256 = "0kc8m9xsl6mbf787zb566swx0dczb9d06zl5y3spiacn89xwdfgl";
      # }))

      pngpaste
      aspell
      aspellDicts.en
      aspellDicts.en-computers

      fd
      fzf
      entr

      # like jq or yq but for HTML
      pup
      jq
      yq
      # [johnkerl/miller: Miller is like awk, sed, cut, join, and sort for name-indexed data such as CSV, TSV, and tabular JSON](https://github.com/johnkerl/miller)
      # miller

      starship
      lsd
      yazi  # https://github.com/sxyazi/yazi

      rlwrap

      tldr  # community-driven man pages

      # bat   # clone of cat
      cloc

      ctop  # htop for docker
      kubectl

      bottom # btm command

      ripgrep
      figlet # show banners

      global

      duf   # modern df
      broot # modern tree
      # file manager
      mc

      readline

      graphviz

      git
      difftastic
      delta

      chezmoi
      fortune
      direnv
      coreutils
      ffmpeg
      gdb
      tree
      # youtube-dl

      watch
      curl
      wget

      gnupg
      # 1.16.4
      go

      mtr
      iftop
      wrk
      pstree

      htop

      # nodejs

      pandoc

      _1password-cli

      # ncdu
      gdu # modern ncdu
      tmux
      tmuxp
      just
      # terminal-notifier # https://github.com/julienXX/terminal-notifier
      # synergy
      # unrar
      # (vscode-with-extensions.override (vscodeOptions // { vscode = pkgs.vscodium; }))

      # Makes copying and pasting in Terminal.app work again on MacOS Sierra
      # https://github.com/tmux/tmux/issues/543#issuecomment-248980734
      # https://github.com/tmux/tmux/issues/543
      reattach-to-user-namespace
      neovim

      # Terms
      # kitty
      oh-my-zsh

      # Java 11
      temurin-bin
      clojure

      pipx
      (python312.withPackages(ps: with ps; [
          pip
          pytest
          black
          isort
          requests
          pytest
          pyflakes
          isort
          tox
          jedi
          json-rpc
          # service-factory
      ]))
  ]
    ++ (with unstable; [
   # Installing manually so far
   # ((emacsPackagesFor emacs30).emacsWithPackages (epkgs: [
   #  epkgs.vterm
   # ]))
  ]);

  programs.man.enable = true;
  # so we can install something like unrar that is free but not OSS
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    # - CVE-2024-2660
    "vault-1.14.10"
  ];
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
    GIT_EXTERNAL_DIFF = "difft";
  };

  environment.systemPath = [
    "$HOME/bin"
    "$HOME/.rd/bin"
    "$HOME/.npm/bin"
    "$HOME/.deno/bin"
    "$HOME/.cargo/bin"
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

    # screenshots of the prompts: https://bneijt.nl/blog/zsh-themes-for-prompts-screenshots/
    promptInit = "autoload -U promptinit && promptinit && prompt suse && setopt prompt_sp";

    loginShellInit = ''

    export PATH=$HOME/.local/bin:/usr/local/sbin:/usr/local/bin:$PATH

    export PIP_DOWNLOAD_CACHE=$HOME/Library/Caches/pip-downloads
    export GPG_TTY=$(tty)

    source ~/functions.sh
    source ~/.env.sh

    function init-nvm() {
      export NVM_DIR="$HOME/.nvm"
      [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    }
    '';
    interactiveShellInit = ''
      PROMPT='[%?] %* %B%~%b £ '
      DISABLE_MAGIC_FUNCTIONS="true"

      # Turn on when measuring plugin performance
      # zmodload zsh/zprof
      HISTSIZE=10000
      SAVEHIST=10000
      setopt autocd # auto cd when only path is entered
      setopt nomatch # throw an error on glob matching nothing

      setopt SHARE_HISTORY

      bindkey -e
      bindkey '\e\e[C' forward-word
      bindkey '\e\e[D' backward-word

      alias route_gprs='sudo rounte delete default; sudo route add default 192.168.44.1'

      eval "$(direnv hook zsh)"
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
    # dive installed via nixpkgs cannot open locally built (not pushed) images
    dive = "docker run -ti --rm  -v /var/run/docker.sock:/var/run/docker.sock wagoodman/dive";

    nixre = "darwin-rebuild switch";
    nixgc = "sudo nix-collect-garbage -d";

    ll = "ls -althF";
    la = "ls -A";
    l = "ls -CF";
    tmux = "tmux -2";
    ddc = "docker-compose";
    vim = "nvim";
    ls  = "lsd";
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
     packages = with pkgs; [
        nerd-fonts.jetbrains-mono
        nerd-fonts.comic-shanns-mono
        nerd-fonts.droid-sans-mono
        nerd-fonts.go-mono
        nerd-fonts.inconsolata
        nerd-fonts.proggy-clean-tt

        nerd-fonts.iosevka
        iosevka-comfy.comfy-wide
        nerd-fonts.iosevka-term

        comic-mono
        dejavu_fonts
        ibm-plex
        proggyfonts
        emacs-all-the-icons-fonts
        go-font
        jetbrains-mono
        fira-code
        # https://github.com/adam7/delugia-code/releases
        # delugia
     ];
   };
}

# Inspiration:
# - https://github.com/ben-z/dotfiles/blob/master/nixpkgs/darwin-configuration.nix

# Interesting list of apps in the Brewfile
# https://github.com/biosan/dotfiles/blob/master/config/macos/Brewfile
