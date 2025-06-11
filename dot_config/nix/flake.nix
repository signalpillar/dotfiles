# nix run nix-darwin/nix-darwin-24.11#darwin-rebuild -- switch --flake ~/.config/nix/flake.nix
{
  description = "samsungPro";

  inputs = {
    # Use `github:NixOS/nixpkgs/nixpkgs-24.11-darwin` to use Nixpkgs 24.11.
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # Use `github:nix-darwin/nix-darwin/nix-darwin-24.11` to use Nixpkgs 24.11.
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs }: 
  let
    configuration = { pkgs, ... }: {
      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment.systemPackages = with pkgs; [
      #  aspell
      #  bandwhich # Bandwidth monitor
      #  bat
      #  bottom # replacement for top
      #  broot # file navigation tool
      #  cargo-audit
      #  cargo-bloat
      #  cargo-edit
      #  cargo-expand # Rust macro expansion utility
      #  cargo-outdated # Find outdated Rust crate dependencies
      #  cargo-make # build tool on top of cargo
      #  cargo-msrv # Find the Minimum Supported Rust Version for a crate
      #  cargo-ndk # Android build support for Rust
      #  cargo-watch # Execute commands when Rust project files change
      #  cargo-workspaces
      #  diesel-cli
      #  dua
      #  dust
      #  emacs
      #  eza # nicer replacement for ls
      #  fd
      #  ffmpeg  # TODO: broken on MacOS, at least for now
      #  hyperfine
      #  lldb
      #  mplayer
      #  nerdfonts
      #  nodejs_21
      #  nushell # A newfangled, more FP-oriented shell
      #  powerline-fonts
      #  resvg
      #  tokei

      # emacs-git

      mpv

      awscli2
      starship
      hadolint           # Dockerfile linter
      # mplayer

      vault
      # devbox # now installed through sh
      lorri

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

      nodejs
      bun

      pandoc

      _1password-cli

      # ncdu
      gdu # modern ncdu
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
      zig
      ];

      system.defaults = {
        dock.autohide = true;
        dock.mru-spaces = false;
        dock.show-recents = false; # Don't show recent applications in the Dock
        finder.AppleShowAllExtensions = true;
        finder.FXPreferredViewStyle = "Nlsv";
        finder.ShowPathbar = true;
        finder.ShowStatusBar = true;
        screencapture.location = "~/Screenshots";
        screensaver.askForPasswordDelay = 1; # Require a password in the screensaver after 1 second
        # universalaccess.reduceTransparency = true;
      };

     # nix.package = pkgs.nix;

     # Cross-compile GNU/Linux binaries, or even full GNU/Linux system images
     nix.linux-builder.enable = true;

     nix.settings.trusted-users = ["volodymyrvitvitskyi"];

     # Necessary for using flakes on this system.
     nix.settings.experimental-features = "nix-command flakes";

     # Create /etc/zshrc that loads the nix-darwin environment.
     programs.zsh.enable = true;  # default shell on catalina
     # programs.fish.enable = true;

     # Enable sudo authentication using fingerprint biometrics
     security.pam.services.sudo_local.touchIdAuth = true;

     # Set Git commit hash for darwin-version.
     system.configurationRevision = self.rev or self.dirtyRev or null;

     # Used for backwards compatibility, please read the changelog before changing.
     # $ darwin-rebuild changelog
     system.stateVersion = 5;

     # The platform the configuration will be used on.
     # aarch64-darwin
     nixpkgs.hostPlatform = "x86_64-darwin";
     nixpkgs.config.allowUnfree = true;
     nixpkgs.overlays = [
	(import (builtins.fetchTarball {
           url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
           sha256 = "03asj6jhdmv611pp970bhbasa8rhc4pmh17cbs080kz7s6h56pxs";
        }))
     ];
    };
  in {
    darwinConfigurations."SamsungPro" = nix-darwin.lib.darwinSystem {
      modules = [ configuration ];
    };
  };
}
