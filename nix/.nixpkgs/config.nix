{
  # You won't be able to install or search for an unfree package as a user,
  # unless you explicitly enable.
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; rec {
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
