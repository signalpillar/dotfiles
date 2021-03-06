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
- [Spark](https://sparkmailapp.com/) or [Mimestream](https://mimestream.com/)
- [Kap](https://getkap.co/)
- mplayer

## iTerm ([source](https://www.mathiaspolligkeit.de/dev/exploring-nix-on-macos/))
iTerm2 has an option to load the preferences from a custom location (iTerm2 →
`Preferences → General → Preferences → Load preferences` from a custom folder or
URL).

You can set the location to `~/.config/iterm2`, click Save Current Settings to
Folder and add the config to the repo.

## Emacs

- [Config of Gergely Nagy](https://github.com/algernon/emacs.d/blob/master/.spacemacs)

## Fonts

- Cascadia Mono ([download](https://github.com/microsoft/cascadia-code))
- IBM Plex ([download](https://github.com/IBM/plex/releases/))

## ML
- https://mlflow.org/

## History

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
