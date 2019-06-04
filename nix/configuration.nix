# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  # imports =
  #   [ # Include the results of the hardware scan.
  #     ./hardware-configuration.nix
  #   ];
  #
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.cleanTmpDir = true;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  fileSystems = [
    { mountPoint = "/";
      label = "nixos";
    }
  ];

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  fonts = {
    fonts = with pkgs; [
      anonymousPro
      corefonts
      dejavu_fonts
      emacs-all-the-icons-fonts
      fira-code
      fira-code-symbols
      #font-droid
      #google-fonts
      inconsolata
      league-of-moveable-type
      liberation_ttf
      powerline-fonts
      proggyfonts
      roboto
      #roboto-mono
      roboto-slab
      source-code-pro
      vistafonts
    ];
  };


  # Set your time zone.
  time.timeZone = "Europe/London";

  programs = {
    bash.enableCompletion = true;
    ssh.startAgent = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "vim";
    };
    systemPackages = with pkgs; [
      docker-compose
      dmenu
      emacs
      file
      firefox
      gcc
      git
      htop
      i3cat
      i3lock-color
      i3lock-fancy
      i3lock-pixeled
      i3status-rust
      libffi
      libnotify
      mplayer
      ncdu
      pkg-config
      (python36.withPackages(ps: with ps; [ pip tox cffi ]))
      tmux
      tree
      unzip
      vim
      wget
      which
      xterm
      tilda
      zeal
    ];
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
  services.virtualbox.enable = true;
  services.locate.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    windowManager.i3.enable = true;
    displayManager.lightdm.enable = true;
    displayManager.sessionCommands =  ''
       xrdb "${pkgs.writeText  "xrdb.conf" ''
          XTerm*faceName:             xft:Dejavu Sans Mono for Powerline:size=11
          XTerm*utf8:                 2
          Xft*dpi:                    149
          ! Xft*antialias:              true
          ! Xft*hinting:                full
          Xft.autohint: 0
          Xft.lcdfilter:  lcddefault
          ! Xft.hintstyle:  hintfull
          ! Xft.hinting: 1
          ! Xft.antialias: 1
       ''}"
    '';
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.vvitvitskyi = {
     isNormalUser = true;
     extraGroups = [ "wheel"  "sudo" "docker" "vboxsf"];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

  nix = {
    autoOptimiseStore = true;
    buildCores = 4;
  };

  # VIRTUALISATION
  virtualisation.docker.enable = true;
  virtualisation.docker.socketActivation = true;




}
