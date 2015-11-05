This project contains all my dotfiles that are moved from box to box.

Running a PYPI mirror, check the following configuration files:

- `$HOME/.pydistutils.cfg`, for `easy_install`
- `$HOME/.pip/pip.conf`, for `pip`

# May 2015

* Moved to [spacemacs](https://github.com/syl20bnr/spacemacs) as a replacement for Prelude.
  It is a Emacs Kit, focused on integration with Evil mode.

* No need to use `nsenter` to enter docker container, using `docker exec` instead. 
