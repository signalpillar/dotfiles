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
        ffmpeg
        gdb
        gitAndTools.gitFull
        jq
        silver-searcher
        vim
      ];
    };
  };
}
