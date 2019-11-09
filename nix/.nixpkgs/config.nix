{
  # You won't be able to install or search for an unfree package as a user,
  # unless you explicitly enable.
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; rec {

    # nix-shell -p lorri ( to test build )
    lorri = (
      let
        src = pkgs.fetchFromGitHub {
              owner  = "target";
              repo   = "lorri";
              rev    = "03f10395943449b1fc5026d3386ab8c94c520ee3";
              sha256 = "0fcl79ndaziwd8d74mk1lsijz34p2inn64b4b4am3wsyk184brzq";
        };
      in pkgs.callPackage src { inherit src; }
    );

    osx = buildEnv {
      name = "osx-desktop";
      paths = [
        aspell
        bc
        coreutils
        # is super handy loading evars when they are exported in .envrc
        direnv
        # Icon fonts for emacs all-the-icons
        emacs-all-the-icons-fonts
        emacsMacport
        ffmpeg
        gdb
        gitAndTools.gitFull
        jq
        lorri
        # used for emacs-terminal integration
        # osx: not supported
        # libvterm
        openssh
        silver-searcher
        tmux
        vim
        unrar
      ];
    };
  };
}
