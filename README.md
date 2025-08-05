# Keep A Changelog for Emacs

# Requirements

Currently only tested on Emacs 30, but it requires only 28.

# Setup

Get the files, load the package and bind a key.

Example using [use-package](https://github.com/jwiegley/use-package),
[elpaca](https://github.com/progfolio/elpaca)

``` emacs-lisp
(use-package keepachangelog
  :elpaca (:type git :host github :repo "xendk/keepachangelog.el")
  :bind ("C-c a" . keepachangelog-add-entry))
```

# Usage

This package makes some assumptions about the format of the changelog
in order to simplify things. Most of these are implied by the example
given by
[keepachangelog.com](https://raw.githubusercontent.com/olivierlacan/keep-a-changelog/refs/heads/main/CHANGELOG.md).

- Lines starting with `## ` are version headers.
- Lines starting with `### ` are sections of type of changes.
