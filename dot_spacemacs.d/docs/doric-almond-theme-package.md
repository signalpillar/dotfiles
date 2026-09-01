# Why Spacemacs asked for `doric-almond-theme`

A short post-mortem from a failed package install on this Spacemacs setup.

The symptom at startup:

```text
--> installing package: doric-almond-theme@dotfile...
Package doric-almond-theme is unavailable. Is the package name misspelled?
```

The archive refresh succeeded.
The package name was wrong.

---

## What failed

`dotspacemacs-themes` listed the theme name `doric-almond`.

```381:383:dot_spacemacs.d/init.el
   dotspacemacs-themes '(
                         (doric-almond :package doric-themes)
                         naysayer
```

The first version of that line was the bare symbol `doric-almond`.

Spacemacs turns each theme into a package to install.
The `@dotfile` tag marks a package that came from `dotspacemacs-themes`.

`spacemacs/get-theme-package-name` does this work.

```342:355:~/.emacs.d/core/core-themes-support.el
(defun spacemacs/get-theme-package-name (theme)
  "Return the package theme for the given THEME name."
  (if-let* (((listp theme))
            (pkg-name (plist-get (cdr theme) :package)))
      pkg-name
    (let ((theme-name (or (car-safe theme) theme)))
      (cond
       ((memq theme-name emacs-built-in-themes) nil)
       ((assq theme-name spacemacs-theme-name-to-package)
        (cdr (assq theme-name spacemacs-theme-name-to-package)))
       (t (intern (format "%S-theme" theme-name)))))))
```

For a bare symbol the function takes the last branch.
`doric-almond` becomes `doric-almond-theme`.

That name is not a package.
GNU ELPA publishes `doric-themes`.
It does not publish `doric-almond-theme`.
`doric-almond` is one theme file inside `doric-themes`.

`dotspacemacs-additional-packages` already listed the real package.

```214:218:dot_spacemacs.d/init.el
   dotspacemacs-additional-packages '(
                                      vs-light-theme
                                      doric-themes
                                      paper-theme
                                      naysayer-theme
```

That list does not stop the theme mapper.
The mapper still queued `doric-almond-theme@dotfile`.
The install then failed on a name that no archive owns.

---

## Why the first guess was wrong

The error looks like a missing archive or a typo.
The refresh line for `nongnu` then looks like the cause.

The refresh was healthy.
The failure sat one stage later: name resolution.

`naysayer`, `paper`, and `vs-light` work as bare symbols.
Each one has a matching `*-theme` package.
`doric-almond` does not.

`doom-acario-light` also lives in a multi-theme package.
Spacemacs already maps it in `spacemacs-theme-name-to-package`.
`doric-almond` has no row in that map.

---

## The fix

Name the package in the theme list.

```emacs-lisp
(doric-almond :package doric-themes)
```

This is the documented form for a theme that does not own its package.

See `doc/DOCUMENTATION.org` in the Spacemacs checkout, section `External theme`.
The example there is `(humanoid-light :package humanoid-themes)`.

Restart Emacs after the edit.
Spacemacs then installs `doric-themes` and loads `doric-almond`.

---

## Rule to keep

If a theme name is not `name` plus `-theme`, write `:package`.
Do not rely on `dotspacemacs-additional-packages` alone.
The theme list still invents a second package name.
