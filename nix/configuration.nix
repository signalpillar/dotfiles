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

networking.hostName = "B-5014";

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

   # openjdk

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

   # ---- Desktop
   brave
   feh
   dropbox
   parcellite

   # ---- Terminals
   kitty
   any-nix-shell
   fortune
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
   aspell
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
                  {x = 1920; y = 1200;}
                  # {x = 1440; y = 900;}
                  # {x = 1920; y = 1200;}
                  # {x = 2560; y = 1440; }
    ];

    # xset r rate 200 25
    autoRepeatDelay = 200;
    layout = "us";

    # By default, the NixOS VirtualBox demo image includes SDDM and Plasma.
    # If you prefer another desktop manager or display manager, you may want
    # to disable the default.
    # Option 1 - KDE
    # enable = true;
    # displayManager.sddm.enable = true;
    # desktopManager.plasma5.enable = true;
    # displayManager.defaultSession = "plasmawayland";

    # Option 2 - i3
    enable = true;
    displayManager.defaultSession = "none+i3";

    windowManager.i3 = {
        enable = true;
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [
            i3-gaps
            i3status # gives you the default i3 status bar
            i3lock #default i3 screen locker
            rofi  # application launcher
         ];
      };

    # Option 3 - i3 & Sway
    # i3 config
    # enable = true;
    # libinput.enable = true;
    # desktopManager.xterm.enable = false;
    # displayManager.lightdm.enable = true;
    # displayManager.defaultSession = "none+i3";
    # windowManager.i3.enable = true;
    # windowManager.i3.configFile = ./i3-config;

    # Option 4 - gnome
    # enable = true;
    # displayManager.gdm.enable = true;
    # desktopManager.gnome.enable = true;
    # services.udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];

    # Option 5 - xfce
    # enable = true;
    # displayManager.gdm.enable = true;
    # desktopManager.xfce.enable = true;
    # displayManager.defaultSession = "xfce";
  };

  programs.sway.enable = true;

  environment.shellAliases = {
    # Use emacsclient to open files in current emacs instance server
    ec = "emacsclient -cn";

    nixre = "darwin-rebuild switch";
    nixgc = "nix-collect-garbage -d";
  };
  programs.ssh.startAgent = true;
  programs.ssh.forwardX11 = true;
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
services.openssh.enable = true;
services.openssh.forwardX11 = true;
# VIRTUALISATION
virtualisation.docker = {
  enable = true;
  autoPrune.enable = true;
};
virtualisation.vmware.guest.enable = true;

sound.enable = true;

environment.etc."i3.conf".text = ''
# Please see http://i3wm.org/docs/userguide.html for a complete reference!

# i3-msg restart
# i3-msg reload

# To see the key names
# nix-shell -p xorg.xev --run "xev -event keyboard"

# https://faq.i3wm.org/question/2569/set-mod-key-only-as-left-mod-key.1.html
set $mod Mod1

# Font for window titles. Will also be used by the bar unless a different font
# is used in the bar {} block below. ISO 10646 = Unicode
font -misc-fixed-medium-r-normal--13-120-75-75-C-70-iso10646-1
# The font above is very space-efficient, that is, it looks good, sharp and
# clear in small sizes. However, if you need a lot of unicode glyphs or
# right-to-left text rendering, you should instead use pango for rendering and
# chose a FreeType font, such as:
# font pango:DejaVu Sans Mono 10

# Use Mouse+$mod to drag floating windows to their wanted position
floating_modifier $mod

# start a terminal
# bindsym $mod+Return exec i3-sensible-terminal
bindsym $mod+Return exec kitty

# kill focused window
bindsym $mod+Shift+q kill

# start dmenu (a program launcher)
bindsym $mod+d exec rofi -show combi
# There also is the (new) i3-dmenu-desktop which only displays applications
# shipping a .desktop file. It is a wrapper around dmenu, so you need that
# installed.
# bindsym $mod+d exec --no-startup-id i3-dmenu-desktop

# change focus
bindsym $mod+j focus left
bindsym $mod+k focus down
bindsym $mod+l focus up
bindsym $mod+semicolon focus right

# alternatively, you can use the cursor keys:
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

# move focused window
bindsym $mod+Shift+j move left
bindsym $mod+Shift+k move down
bindsym $mod+Shift+l move up
bindsym $mod+Shift+semicolon move right

# alternatively, you can use the cursor keys:
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right

# split in horizontal orientation
bindsym $mod+h split h

# split in vertical orientation
bindsym $mod+v split v

# enter fullscreen mode for the focused container
bindsym $mod+f fullscreen

# change container layout (stacked, tabbed, toggle split)
bindsym $mod+s layout stacking
bindsym $mod+w layout tabbed
bindsym $mod+e layout toggle split

# toggle tiling / floating
bindsym $mod+Shift+space floating toggle

# change focus between tiling / floating windows
bindsym $mod+space focus mode_toggle

# focus the parent container
bindsym $mod+a focus parent

# focus the child container
#bindsym $mod+d focus child

# switch to workspace
bindsym $mod+1 workspace 1
bindsym $mod+2 workspace 2
bindsym $mod+3 workspace 3
bindsym $mod+4 workspace 4
bindsym $mod+5 workspace 5
bindsym $mod+6 workspace 6
bindsym $mod+7 workspace 7
bindsym $mod+8 workspace 8
bindsym $mod+9 workspace 9
bindsym $mod+0 workspace 10

# move focused container to workspace
bindsym $mod+Shift+1 move container to workspace 1
bindsym $mod+Shift+2 move container to workspace 2
bindsym $mod+Shift+3 move container to workspace 3
bindsym $mod+Shift+4 move container to workspace 4
bindsym $mod+Shift+5 move container to workspace 5
bindsym $mod+Shift+6 move container to workspace 6
bindsym $mod+Shift+7 move container to workspace 7
bindsym $mod+Shift+8 move container to workspace 8
bindsym $mod+Shift+9 move container to workspace 9
bindsym $mod+Shift+0 move container to workspace 10

# reload the configuration file
bindsym $mod+Shift+c reload
# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $mod+Shift+r restart
# exit i3 (logs you out of your X session)
bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"

# resize window (you can also use the mouse for that)
mode "resize" {
        # These bindings trigger as soon as you enter the resize mode

        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym j resize shrink width 10 px or 10 ppt
        bindsym k resize grow height 10 px or 10 ppt
        bindsym l resize shrink height 10 px or 10 ppt
        bindsym semicolon resize grow width 10 px or 10 ppt

        # same bindings, but for the arrow keys
        bindsym Left resize shrink width 10 px or 10 ppt
        bindsym Down resize grow height 10 px or 10 ppt
        bindsym Up resize shrink height 10 px or 10 ppt
        bindsym Right resize grow width 10 px or 10 ppt

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
}

bindsym $mod+r mode "resize"

# Start i3bar to display a workspace bar (plus the system information i3status
# finds out, if available)
bar {
        position top
        status_command i3status
        font termsyn:monospace 8
}

# exec dropbox start &
# exec guake -name "guake-x" &
# exec nm-applet &
exec parcellite &
exec dropbox &
exec kitty &
exec feh --randomize --bg-fill ~/Pictures/* &

for_window [window_role="pop-up"] floating enable
# for_window [title="Guake!"] floating enable
# Gaps configuragion
for_window [class=".*"] border pixel 1

gaps inner 10
gaps outer 10
'';
}
