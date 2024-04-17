{ pkgs, ... }: {
  home.username = "ubuntu";
  home.homeDirectory = "/home/ubuntu";
  home.stateVersion = "23.11";
  programs.home-manager.enable = true;

  imports = [
    ./packages.nix
  ];

  nixpkgs.config = {
    allowUnfree = true;
    allowUnfreePredicate = (_: true);
    allowBroken = false;
  };

  home.packages = [
    pkgs.btop
  ];

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
      # for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
      HISTSIZE = 1000;
      HISTFILESIZE = 2000;
    };
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
  };
}
