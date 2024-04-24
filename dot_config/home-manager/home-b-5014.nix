{ pkgs, ... }:
let
  inherit (pkgs.stdenv) isDarwin isLinux;
in {
  home.username = "volodymyrvitvitskyi";
  home.homeDirectory = "/Users/volodymyrvitvitskyi";
  home.stateVersion = "23.11";
  programs.home-manager.enable = true;

  imports = [
    ./packages.nix
  ];

  nixpkgs.config = {
    allowUnfree = true;
    allowUnfreePredicate = (_: true);
    allowBroken = false;
    overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        sha256 = "0jxhbwsx1pb2y4rw9ycripm65kcjf25r39qav22hfv5606szl102";
      }))
    ];
  };

  # https://github.com/wimpysworld/nix-config/blob/main/home-manager/default.nix
  home.packages = [
    pkgs.btop
    ((pkgs.emacsPackagesFor pkgs.emacs29-nox).emacsWithPackages (epkgs: [
     epkgs.vterm
    ]))

    pkgs.terraform
    pkgs.vault
    pkgs.devbox

    # (import (fetchTarball {
    #   url = https://github.com/cachix/devenv/archive/refs/tags/v1.0.1.tar.gz;
    #   sha256 = "0kc8m9xsl6mbf787zb566swx0dczb9d06zl5y3spiacn89xwdfgl";
    # }))
    pkgs.lorri

    pkgs.babashka

    pkgs.aspell
    pkgs.aspellDicts.en
    pkgs.aspellDicts.en-computers
    pkgs.rlwrap
    pkgs.cloc
    pkgs.ctop
    pkgs.kubectl
    pkgs.global
    pkgs.mc
    pkgs.readline
    pkgs.graphviz
    pkgs.diffstatic
    pkgs.delta
    pkgs.ffmpeg
    pkgs.gdb
    pkgs.tree
    pkgs.watch
    pkgs.curl
    pkgs.gnupg
    pkgs.wrk
    pkgs.pstree
    pkgs.pandoc
    pkgs._1password
    pkgs.unrar
    # Makes copying and pasting in Terminal.app work again on MacOS Sierra
    # https://github.com/tmux/tmux/issues/543#issuecomment-248980734
    # https://github.com/tmux/tmux/issues/543
    pkgs.reattach-to-user-namespace
    pkgs.neovim

    # Java 11
    pkgs.adoptopenjdk-bin
    pkgs.pipx
    (pkgs.python39.withPackages(ps: with ps; [
        pip
        pytest
        black
        isort
        requests
        pytest
        pyflakes
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

  programs = {
    man.enable = true;
    nix-index.enable = true;
    yt-dlp = {
      enable = true;
      package = pkgs.yt-dlp;
      settings ={
        audio-format = "best";
        audio-quality = 0;
        embed-chapters = true;
        embed-metadata = true;
        embed-subs = true;
        embed-thumbnail = true;
        remux-video = "aac>m4a/mov>mp4/mkv";
        sponsorblock-mark = "sponsor";
        sub-langs = "all";
      };
    };
  };
  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    defaultCommand = "rg --files --hidden --follow";
    defaultOptions = [ "-m --bind ctrl-a:select-all,ctrl-d:deselect-all" ];
  };

  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    secureSocket = false;
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.eza = { enable = true; };

  # TODO: there is an option to specify .bashrc / .bashprofile
  # See https://github.com/Arkham/dotfiles.nix/blob/7b572c59f55e33833301a3e29b8485c823a13009/rest.nix
  programs.bash = {
    enable = true;
    profileExtra = builtins.readFile ./profile;
    # don't put duplicate lines in the history. See bash(1) for more options
    # ... or force ignoredups and ignorespace
    historyControl = [ "ignorespace" "ignoredups" ];
    sessionVariables = {
      PROMPT_COMMAND = "echo";
      EDITOR = "vim";
      SHELL = "${pkgs.bashInteractive}/bin/bash";
    };
    historySize = 100000;
    historyFileSize = 4200000;
    shellAliases = {
      ll = "eza -alF";
      ls = "eza -F";
      la = "eza -A";
      rm = "rm -i";
      mv = "mv -i";
      cp = "cp -i";
      grep = "grep --color=auto";
      sudo = "sudo ";
      ddc = "docker-compose";
      ex  = "emacs -nw";
    };
    shellOptions = [
      "cdspell"
      # check the window size after each command and, if necessary,
      # update the values of LINES and COLUMNS.
      "checkwinsize"
      "cmdhist"
      "dotglob"
      # append to the history file, don't overwrite it
      "histappend"
      "nocaseglob"
    ];
    initExtra = builtins.readFile ./bashrc;
    historyIgnore = [
      "cd"
      "pushd"
      "popd"
      "z"
      "ls"
      "ll"
      "la"
      "rm"
      "rmdir"
      "git show"
      "tldr"
      "exit"
    ];
    logoutExtra = ''
      # when leaving the console clear the screen to increase privacy

      if [ "$SHLVL" = 1 ]; then
        [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
      fi
    '';
  };
}
