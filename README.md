This project contains all my dotfiles that are moved from box to box.

Manually installed

- [Raycast](https://www.raycast.com/)
- [AltTab - Windows alt-tab on macOS](https://alt-tab-macos.netlify.app/)
- [exelban/stats: macOS system monitor in your menu bar](https://github.com/exelban/stats)
- [MeetingBar - Simplify Meetings on macOS with One-Click Access](https://meetingbar.app/)
- [dwarvesf/hidden: An ultra-light MacOS utility that helps hide menu bar icons](https://github.com/dwarvesf/hidden)
- [Nix & NixOS | Reproducible builds and deployments](https://nixos.org/)
- Brave browser
- [AppCleaner](https://freemacsoft.net/appcleaner/)
- [kitty](https://sw.kovidgoyal.net/kitty/)
- Signal
- Slack
- Clipy
- Docker
- VirtualBox
- [Logseq: A privacy-first, open-source knowledge base](https://logseq.com/)
- Dropbox
- [Android Command line tools](https://developer.android.com/studio#cmdline-tools)
- [super-productivity](https://github.com/johannesjo/super-productivity)
- [Spark](https://sparkmailapp.com/) or [Mimestream](https://mimestream.com/)
- [Kap](https://getkap.co/)
- mplayer
- [Hammerspoon](https://www.hammerspoon.org/)
- https://matthewpalmer.net/vanilla/
- https://shottr.cc/ (Best screeshot software)
- [Proxyman · Debug, intercept & mock HTTP with Proxyman](https://proxyman.io/)

## AI
- [GitHub - j178/chatgpt: An elegant interactive CLI for ChatGPT](https://github.com/j178/chatgpt)

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

## Maybe Later
- [sq | wrangle data](https://sq.io/) (like jq/yq but for SQL)

## History

# June 2023
nix-shell and darwin-nix

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
