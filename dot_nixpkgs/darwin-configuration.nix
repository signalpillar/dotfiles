{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs;
    [
      aspellDicts.en

      tldr  # community-driven man pages

      bat   # clone of cat
      cloc

      dive  # tool to explore each layer of the docker image

      ag
      ripgrep
      exa
      figlet # show banners

      global

      # file manager
      mc

      readline

      graphviz

      git
      chezmoi
      fortune
      direnv
      coreutils
      ffmpeg
      gdb
      jq
      tree
      youtube-dl

      watch

      # 1.16.4
      go

      mtr
      iftop
      wrk
      pstree

      htop

      nodejs

      _1password

      ncdu
      tmux
      just
      # terminal-notifier # https://github.com/julienXX/terminal-notifier
      # synergy
      unrar
      # (vscode-with-extensions.override (vscodeOptions // { vscode = pkgs.vscodium; }))

      # Makes copying and pasting in Terminal.app work again on MacOS Sierra
      # https://github.com/tmux/tmux/issues/543#issuecomment-248980734
      # https://github.com/tmux/tmux/issues/543
      reattach-to-user-namespace

      # Editors
      (
        pkgs.neovim.override {
          vimAlias = true;
          configure = {
            packages.myPlugins = with pkgs.vimPlugins; {
              start = [
                vim-lastplace
                vim-nix
                nerdcommenter #preservim/nerdcommenter
                vim-sleuth #tpope/vim-sleuth
                vim-surround #tpope/vim-surround
                vim-test #janko/vim-test
              ];
              opt = [];
            };
            # customRC = builtins.readFile ./../dotfiles/.vimrc;
          };
        }
      )
      emacsMacport
      emacs-all-the-icons-fonts

      # Terms
      kitty
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
  };

  programs.man.enable = true;
  # so we can install something like unrar that is free but not OSS
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = false;

  environment.variables = {
    EDITOR = "vim";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    # source: http://itchyknowsdevs.me/blog/developing-golang-in-nixos/
    GOROOT = [ "${pkgs.go.out}/share/go" ];
  };

  environment.systemPath = [
    "$HOME/bin"
    "$HOME/.npm/bin"
    "$HOME/mutable_node_modules/bin"
  ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    # oh-my-zsh.enable = true;
    # https://github.com/nix-community/home-manager/issues/1226
    # ohMyZsh = {
    #   enable = true;
    #   customPkgs = [];
    #   theme = "amuse";
    #   # zsh-navigation-tools
    #   plugins = [ "git" "tmux" "z" "docker" "colored-man-pages" "zsh-autosuggestions"];
    # };
    # interactiveShellInit = ''
    #   source "$(fzf-share)/key-bindings.zsh"
    #   source "$(fzf-share)/completion.zsh"
    #   eval "$(direnv hook zsh)"
    #   export DIRENV_LOG_FORMAT= # Silence direnv
    # '';
    # promptInit = ''
    #   any-nix-shell zsh --info-right | source /dev/stdin
    # '';
    # syntaxHighlighting.enable = true;
  };

  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  environment.shellAliases = {
    # Use emacsclient to open files in current emacs instance server
    ec = "emacsclient -cn";

    nixre = "darwin-rebuild switch";
    nixgc = "nix-collect-garbage -d";
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
    NSGlobalDomain.NSWindowResizeTime = "0.001";

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
     enableFontDir = true;
     fonts = with pkgs; [
        dejavu_fonts
        ibm-plex
        inconsolata
        proggyfonts
     ];
   };
}

# Inspiration:
# - https://github.com/ben-z/dotfiles/blob/master/nixpkgs/darwin-configuration.nix
