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
  :bind ("C-c a" . keepachangelog-open)
        ("C-c A" . keepachangelog-add-entry))
```

# Usage

Create a `CHANGELOG.md` in the root of your project and ensure there's
at least one version header. Now, from any file of the project you may
call `keepachangelog-open` to open the changelog, or
`keepachangelog-add-entry` to open and add a new entry.

# Assumptions

This package makes some assumptions about the format of the changelog
in order to simplify things. Most of these are implied by the example
given by
[keepachangelog.com](https://raw.githubusercontent.com/olivierlacan/keep-a-changelog/refs/heads/main/CHANGELOG.md),
with some allowances.

- Lines starting with `## ` are version headers.
- Lines starting with `### ` are sections of type of changes.
- Versions and sections are separated by an empty line.
- Section title and its items *may* be separated by a newline.
  keepachangelog.com suggest (by example) a newline, while the author
  of this package thinks it reads better in raw without.
