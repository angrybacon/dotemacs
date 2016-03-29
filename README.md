# DotEmacs

This repository contains my personal Emacs configuration. It was originally meant for personal uses
and to help me keep track of my Emacs files accross several machines. I've kept this repository
public for other Emacs lovers.

Have fun tinkering : j

# Screenshots

My Emacs configuration is a living mixture of files that are modified on a daily basis. As such,
the following screenshots might not reflect `:master`.

![dotemacs.2016-03-14.png](http://i.imgur.com/BN8dIS9l.png)

More coming *soon*™

# Features

Below are the most notable features supported:

- Appearance

    - [`zenburn-theme`][zenburn-theme]: non-aggressive palette and pastel tones with
      [Zenburn][zenburn]
    - [`powerline`][powerline]: prettier mode line

- Languages

    - [`company`][company]: autocompletion for most languages
    - [`flycheck`][flycheck]: lint errors, warnings and notes
    - [`tern`][tern]: code analysis and error reporting for JavaScript

- Goodies

    - [`emmet-mode`][emmet-mode]: support for [Emmet][emmet] on HTML and CSS-like files
    - [`helm`][helm]: incremental selection framework to fuzzily narrow down choices
    - [`magit`][magit]: git integration
    - [`multiple-cursors`][multiple-cursors]: support for multiple cursors
    - [`projectile`][projectile]: project interface to issue actions within a project structure
      (eg. replace, search, grep)
    - [`smartparens`][smartparens]: supposedly smarter parentheses (still looking for a better
      implementation though)

And many more.

For more information about how things works, read the source Luke!
For each package, I have left a link to their website.

[company]: https://github.com/company-mode/company-mode
[emmet]: http://emmet.io/
[emmet-mode]: https://github.com/smihica/emmet-mode
[flycheck]: https://github.com/flycheck/flycheck
[helm]: https://github.com/emacs-helm/helm
[magit]: https://github.com/magit/magit
[multiple-cursors]: https://github.com/magnars/multiple-cursors.el
[powerline]: https://github.com/milkypostman/powerline
[projectile]: https://github.com/bbatsov/projectile
[smartparens]: https://github.com/Fuco1/smartparens
[tern]: http://ternjs.net/doc/manual.html#emacs
[zenburn]: http://kippura.org/zenburnpage/
[zenburn-theme]: https://github.com/bbatsov/zenburn-emacs

# Installation

## Emacs

1. I use [Mitsuharu Yamamoto's Mac port][github-emacs]. The configuration files *should*
   work with any build nonetheless. In any case, install Emacs 24 or above.
1. Clone `dotemacs/` into `~/.emacs.d/`.
1. Edit `~/.emacs.d/lisp/init-constants.el` to suit your needs.

[github-emacs]: https://github.com/railwaycat/homebrew-emacsmacport

## Flycheck

I use [`flycheck`][flycheck] as linter framework to display syntax warnings and errors whithin each
buffer. See below for the linters I personally use:

```bash
# Python
pip install flake8

# SCSS
gem install scss_lint scss_lint_reporter_checkstyle
```

You can configure the way linters check buffers with their own configuration files. You may refer
to their very documentation.

- `flake8`: `~/.flake8rc`
- `scss-lint`: `~/.scss-lint.yml`

[flycheck]: https://github.com/flycheck/flycheck
