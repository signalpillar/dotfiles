{ pkgs, ... }:
let
  inherit (pkgs.stdenv) isDarwin isLinux;
in {
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
    pkgs.nodePackages.jsonlint
  ];

  programs = {
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
    terminal = "xterm-256color";
    secureSocket = false;
    # Lowers the delay time between the prefix key and other keys - fixes pausing in vim
    # taken from https://gist.github.com/NickLaMuro/1687643
    escapeTime = 1;
    # scrollback buffer n lines
    historyLimit = 10000;

    mouse = true;

    extraConfig = ''
    # UTF8 is autodetected in 2.2 onwards, but errors if explicitly set
    # some configuration is taken from https://gist.github.com/NickLaMuro/1687643
    new-session -n $HOST

    set -g status on
    set -g status-keys emacs
    # set-option -g default-shell /bin/zsh

    set-window-option -g mode-keys vi

    # set-window-option xterm-keys on
    # unbind C-b

    set -g prefix C-b

    # Open a new window and pane with the PWD of the pane where creation was initiated
    bind '"' split-window -c "#{pane_current_path}"
    bind % split-window -h -c "#{pane_current_path}"
    bind c new-window -c "#{pane_current_path}"

    # Use vim keybindings in copy mode
    setw -g mode-keys vi

    # vim-like pane switching
    bind -r k select-pane -U
    bind -r j select-pane -D
    bind -r h select-pane -L
    bind -r l select-pane -R

    # # Setup 'v' to begin selection as in Vim
    bind-key -T copy-mode-vi 'v' send -X begin-selection
    bind-key -T copy-mode-vi 'y' send -X copy-pipe "xclip -in -selection clipboard"


    # # Update default binding of `Enter` to also use copy-pipe
    unbind -T copy-mode-vi Enter

    set-option -ga terminal-overrides ',*:enacs@:smacs@:rmacs@:acsc@'
    set -ga terminal-overrides ",xterm-256color:Tc"

    # http://unix.stackexchange.com/questions/1045/getting-256-colors-to-work-in-tmux
    # set -g default-terminal "screen-256color-italic"
    set -g default-terminal "xterm-256color"

    bind-key          S choose-window "join-pane -v -s "%%""
    bind-key          V choose-window "join-pane -h -s "%%""
    bind-key          l last-window

    ## set window notifications
    set-option -g visual-activity on
    set-window-option -g monitor-activity on
    set-window-option -g automatic-rename off


    # Theme configuration
    # set status bar
    # -----------------------------------------------------
    # https://github.com/jimeh/tmux-themepack/blob/master/basic.tmuxtheme
    # Status update interval
    set -g status-interval 1

    # Basic status bar colors
    set -g status-style bg=black,fg=cyan

    # Left side of status bar
    set -g status-left-style bg=black,fg=green
    set -g status-left-length 40
    set -g status-left "#S #[fg=white]» #[fg=yellow]#I #[fg=cyan]#P"

    # Right side of status bar
    set -g status-right-style bg=black,fg=cyan
    set -g status-right-length 40
    set -g status-right "« #[fg=yellow]%H:%M:%S #[fg=green]%d-%b-%y"

    # Window status
    set -g window-status-format " #I:#W#F "
    set -g window-status-current-format " #I:#W#F "

    # Current window status
    set -g window-status-current-style bg=red,fg=black

    # Window with activity status
    set -g window-status-activity-style bg=black,fg=yellow

    # Window separator
    set -g window-status-separator ""

    # Window status alignment
    set -g status-justify centre

    # Pane border
    set -g pane-border-style bg=default,fg=default

    # Active pane border
    set -g pane-active-border-style bg=default,fg=green

    # Pane number indicator
    set -g display-panes-colour colour233
    set -g display-panes-active-colour colour245

    # Clock mode
    # set -g clock-mode-colour red
    # set -g clock-mode-style 24

    # Message
    set -g message-style bg=default,fg=default

    # Command message
    set -g message-command-style bg=default,fg=default

    # Mode
    set -g mode-style bg=red,fg=default
    '';

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
