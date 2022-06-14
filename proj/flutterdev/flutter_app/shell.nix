# https://github.com/tadfisher/android-nixpkgs
{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  android-nixpkgs = callPackage (import (builtins.fetchGit {
    url = "https://github.com/tadfisher/android-nixpkgs.git";
  })) {
    # Default; can also choose "beta", "preview", or "canary".
    channel = "stable";
  };

  android-sdk = android-nixpkgs.sdk (sdkPkgs: with sdkPkgs; [
    cmdline-tools-latest
    build-tools-32-0-0
    platform-tools
    platforms-android-31
    emulator
    system-images-android-31-google-apis-playstore-x86-64
  ]);

in
mkShell {
  buildInputs = [
    # sdkmanager --list_installed
    # avdmanager --verbose create avd -n haveanumber -k "system-images;android-31;google_apis_playstore;x86_64" --device "pixel"
    # emulator -avd android-32
    android-sdk
  ];
}
