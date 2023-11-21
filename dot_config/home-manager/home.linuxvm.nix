{ config, pkgs, ... }:

{
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
    };
  };
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "volod";
  home.homeDirectory = "/home/volod";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
      terraform
      vault
      lorri
      babashka

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
      eza   # Former exa
      figlet # show banners

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

      watch
      curl
      wget

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

      neovim

      # Editors
     ((emacsPackagesFor emacs29).emacsWithPackages (epkgs: [
      epkgs.vterm
     ]))

      # Java 11
      adoptopenjdk-bin
      clojure

      (python312.withPackages(ps: with ps; [
      ]))

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/volod/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
