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
