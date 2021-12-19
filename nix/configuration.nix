{ config, lib, pkgs, ... }:

{
#  imports = [ <nixpkgs/nixos/modules/installer/virtualbox-demo.nix> ];
# ---------------------------------------------------------
imports = [
<nixpkgs/nixos/modules/virtualisation/virtualbox-image.nix>
];
# FIXME: UUID detection is currently broken
boot.loader.grub.fsIdentifier = "provided";
# Add some more video drivers to give X11 a shot at working in
# VMware and QEMU.
services.xserver.videoDrivers = lib.mkOverride 40 [ "vmware" "cirrus" "vesa" "modesetting" ];

powerManagement.enable = false;
system.stateVersion = lib.mkDefault "18.03";

# ---------------------------------------------------------

# Enable GDM/GNOME by uncommenting above two lines and two lines below.
# services.xserver.displayManager.gdm.enable = true;
# services.xserver.desktopManager.gnome3.enable = true;
# services.xserver.desktopManager.gnome.enable = true;
# services.xserver.displayManager.gdm.enable = true;
# services.gnome.core-utilities.enable = false;

# Set your time zone.
# time.timeZone = "Europe/Amsterdam";
system.autoUpgrade.channel = https://nixos.org/channels/nixos-21.11;
system.autoUpgrade.enable = true;

# Define a user account. Don't forget to set a password with ‘passwd’.
users.users.demo = {
   isNormalUser = true;
   shell = pkgs.zsh;
   extraGroups = [ "wheel"  "sudo" "docker" "vboxsf"];
};

# List packages installed in system profile. To search, run:
# \$ nix search wget
environment.systemPackages = with pkgs; [
   ag
   chezmoi
   cmake
   direnv
   file
   fzf
   gcc
   git
   gnumake
   htop
   libffi
   libnotify
   lshw
   mplayer
   ncdu

   nodejs

   openjdk

   openssl
   pkg-config
   tmux
   tree
   unzip
   zeal
   zathura
   xclip
   which
   wget

   zsh
   oh-my-zsh

   gccStdenv

   tldr  # community-driven man pages
   bat   # clone of cat

   stdenv.cc.cc.lib

   # ---- Configure
  # ARandR is designed to provide a simple visual front end for XRandR. Relative monitor positions are shown graphically and can be changed in a drag-and-drop way.
   # arandr
   fortune

   # ---- Desktop
   brave
   feh
   dropbox
   parcellite

   # ---- Terminals
   kitty
   any-nix-shell
   nushell

   # ---- Python
  (python39.withPackages(ps: with ps; [
      pip
      black
      isort
  ]))

   # ---- Docker & K8S
   #  teleport version 4.2.11 is needed to make it work with shipcat
   (import (builtins.fetchTarball {
     name = "nixos-21.05";
     url = "https://github.com/NixOS/nixpkgs/archive/2fed8df61dc5f7a0f728947706c5114c36f04497.tar.gz";
     # Hash obtained using `nix-prefetch-url --unpack <url>`
     sha256 = "0n979s0v9wfn21fl5q3g0p58l85c35s7ajvq3cmzv0i8l8lfyghj";
   }) {}).teleport
   kubectl
   dive  # tool to explore each layer of the docker image
   docker
   docker-compose

   # ---- Editors & Co
   aspellDicts.en
   graphviz
   global

   ((emacsPackagesNgGen emacs27).emacsWithPackages (epkgs: [
     epkgs.vterm
   ]))
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
];

  nix = {
    autoOptimiseStore = true;
    gc.automatic = false;
    optimise.automatic = true;
  };

  documentation.man.enable = true;
  # so we can install something like unrar that is free but not OSS
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = false;
  nixpkgs.config.permittedInsecurePackages = [
    "electron-9.4.4"
  ];


  environment.variables = {
    EDITOR = "vim";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    # source: http://itchyknowsdevs.me/blog/developing-golang-in-nixos/
    GOROOT = [ "${pkgs.go.out}/share/go" ];
    TERMINAL = "kitty";
  };

  # systemPath is not supported
  # environment.systemPath = [
  #   "$HOME/bin"
  #   "$HOME/.npm/bin"
  #   "$HOME/mutable_node_modules/bin"
  # ];

  # the link is required for i3
  # see https://nixos.wiki/wiki/I3
  environment.pathsToLink = [ "/libexec" ];

  services.xserver = {
    resolutions = [
      # {x = 1440; y = 900;}
      {x = 2560; y = 1600;}
    ];

    # xset r rate 200 25
    autoRepeatDelay = 200;
    layout = "us";
   # displayManager.defaultSession = "gnome";
   # windowManager.i3 = {
   #     enable = true;
   #     package = pkgs.i3-gaps;
   #     extraPackages = with pkgs; [
   #         i3status # gives you the default i3 status bar
   #         i3lock #default i3 screen locker
   #         rofi  # application launcher
   #      ];
   #   };
  };

  # programs.sway.enable = true;

  environment.shellAliases = {
    # Use emacsclient to open files in current emacs instance server
    ec = "emacsclient -cn";

    nixre = "darwin-rebuild switch";
    nixgc = "nix-collect-garbage -d";
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    # the paste is very slow with this option enabled
    # autosuggestions.enable = true;
    # https://github.com/nix-community/home-manager/issues/1226
    ohMyZsh = {
      enable = true;
      customPkgs = [];
      theme = "amuse";
      # zsh-navigation-tools
      plugins = [ "git" "tmux" "z" "docker" "colored-man-pages"];
    };
    interactiveShellInit = ''
      source "$(fzf-share)/key-bindings.zsh"
      source "$(fzf-share)/completion.zsh"
      eval "$(direnv hook zsh)"
      export DIRENV_LOG_FORMAT= # Silence direnv
    '';
    promptInit = ''
      any-nix-shell zsh --info-right | source /dev/stdin
    '';
    syntaxHighlighting.enable = true;
  };

  fonts = {
     fontDir.enable = true;
     fontconfig.cache32Bit = true;
     fonts = with pkgs; [
        dejavu_fonts
        go-font
        ibm-plex
        inconsolata
        proggyfonts
        emacs-all-the-icons-fonts
     ];
   };

# Enable the OpenSSH daemon.
# services.openssh.enable = true;
# VIRTUALISATION
virtualisation.docker.enable = true;
virtualisation.vmware.guest.enable = true;
}
