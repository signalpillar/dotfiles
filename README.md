This project contains all my dotfiles that are moved from box to box.

Manually installed

- Appcleaner
- iTerm
- Brave browser
- Signal
- Slack
- Clipy
- Docker
- VirtualBox
- Joplin
- Dropbox
- Karabine-Elements
  - It stopped working in Big Sur
    - Use internal tool to generate the mapping file
      - [Great Blog Post from Rakhesh Sasidharan](https://rakhesh.com/mac/using-hidutil-to-map-macos-keyboard-keys/)
      - [Remapping File Generator](https://hidutil-generator.netlify.app/)
- [Android Command line tools](https://developer.android.com/studio#cmdline-tools)
- [super-productivity](https://github.com/johannesjo/super-productivity)
- [Spark](https://sparkmailapp.com/) or [Mimestream](https://mimestream.com/)
- [Kap](https://getkap.co/)
- mplayer

## iTerm ([source](https://www.mathiaspolligkeit.de/dev/exploring-nix-on-macos/))
iTerm2 has an option to load the preferences from a custom location (iTerm2 →
`Preferences → General → Preferences → Load preferences` from a custom folder or
URL).

You can set the location to `~/.config/iterm2`, click Save Current Settings to
Folder and add the config to the repo.

## Android Command line tools
- Download and unpack the dir `~/proj/cmdline-tools/latest`.
- Add to path
- Run `sdkmanager`

## Emacs

- [Config of Gergely Nagy](https://github.com/algernon/emacs.d/blob/master/.spacemacs)

## Fonts

- Cascadia Mono ([download](https://github.com/microsoft/cascadia-code))
- IBM Plex ([download](https://github.com/IBM/plex/releases/))

## ML
- https://mlflow.org/

## Nix

- [Intro to flakes](https://serokell.io/blog/practical-nix-flakes)
- [Tutorial](https://www.tweag.io/blog/2020-05-25-flakes/)
  - `nix-env -f '<nixpkgs>' -iA nixUnstable`
- [NixOS in VMWare Fusion](https://dev.to/ryuheechul/quickest-way-to-run-nixos-on-your-vmware-fusion-4dn7)

## History

# Sept 2021
Development happens only in NixOS running in VMWare Fusion. I feel I am far from
using flake though.

# March 2021
Slow migration to nix-darwin started.
All non-gui apps can be there.

# Jan 2020
Project restructured to work with [chezmoi](https://github.com/twpayne/chezmoi/blob/master/docs/HOWTO.md)

# June 2019
An attempt to adopt Nix package manager.

# May 2015

* Moved to [spacemacs](https://github.com/syl20bnr/spacemacs) as a replacement for Prelude.
  It is a Emacs Kit, focused on integration with Evil mode.

* No need to use `nsenter` to enter docker container, using `docker exec` instead. 

# NixOS

- `xset r rate 200 25`
- `npm config set prefix '~/mutable_node_modules'`
