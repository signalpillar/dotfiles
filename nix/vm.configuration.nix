{ config, pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/installer/virtualbox-demo.nix> ];

  # Let demo build as a trusted user.
# nix.trustedUsers = [ "demo" ];

# Mount a VirtualBox shared folder.
# This is configurable in the VirtualBox menu at
# Machine / Settings / Shared Folders.
fileSystems."/mnt/host" = {
  fsType = "vboxsf";
  device = "volodymyr.vitvitskyi";
  options = [ "rw" ];
};

# By default, the NixOS VirtualBox demo image includes SDDM and Plasma.
# If you prefer another desktop manager or display manager, you may want
# to disable the default.
# services.xserver.desktopManager.plasma5.enable = lib.mkForce false;
# services.xserver.displayManager.sddm.enable = lib.mkForce false;

# Enable GDM/GNOME by uncommenting above two lines and two lines below.
# services.xserver.displayManager.gdm.enable = true;
# services.xserver.desktopManager.gnome3.enable = true;

# Set your time zone.
# time.timeZone = "Europe/Amsterdam";

services.xserver = {
  enable = true;
  desktopManager = {
    xterm.enable = false;
  };
  displayManager = {
    defaultSession = "none+i3";
  };
  windowManager.i3 = {
    enable = true;
    extraPackages = with pkgs; [
      dmenu
      i3status
      i3lock
      i3blocks
    ];
  };
};

# Define a user account. Don't forget to set a password with ‘passwd’.
users.users.demo = {
   isNormalUser = true;
   extraGroups = [ "wheel"  "sudo" "docker" "vboxsf"];
};


# # links /libexec from derivations to /run/current-system/sw 
environment.pathsToLink = [ "/libexec" ];
# Based on the i3-sensible-terminal source, it respects $TERMINAL evar as highest-priority.
environment.sessionVariables.TERMINAL = [ "kitty" ];

# List packages installed in system profile. To search, run:
# \$ nix search wget
environment.systemPackages = with pkgs; [
   ag
   chezmoi
   cmake
   direnv
   docker
   docker-compose
   ((emacsPackagesNgGen emacs27).emacsWithPackages (epkgs: [
     epkgs.vterm
   ]))
   file
   fzf
   gcc
   git
   global
   gnumake
   htop
   kitty
   libffi
   libnotify
   lshw
   mplayer
   ncdu
   openssl
   pkg-config
   tmux
   tree
   unzip
   vim
   zeal
   zathura
   xclip
   termite
   which
   wget
   zsh
];


programs.ssh.startAgent = true;

# Enable the OpenSSH daemon.
# services.openssh.enable = true;
# VIRTUALISATION
virtualisation.docker.enable = true;
virtualisation.virtualbox.guest.enable = true;


fonts.fonts = with pkgs; [
  emacs-all-the-icons-fonts
  hermit
  source-code-pro
  terminus_font
];

}
