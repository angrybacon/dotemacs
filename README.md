DotEmacs
========

This repository contains my personal Emacs configuration. It was originally meant for personal uses
and to help me keep track of my Emacs files accross several machines. I've kept this repository
public for other Emacs lovers.

Have fun tinkering : j

Screenshots
===========

My Emacs configuration is a living mixture of files and is modified on a daily basis. As such, the
following screenshots might not reflect `:master`.

![dotemacs.2016-03-14.png](http://i.imgur.com/eZcEvzOl.png)

More coming *soon*™

Features
========

Below are the most notable features supported:

- Theming

    - [`zenburn-theme`][zenburn-theme]: Non-aggressive palette and pastel tones with
      [Zenburn][zenburn]
    - [`powerline`][powerline]: Prettify the mode line with a hand-made template (major mode,
      buffer name with semantic colors, project name, VC branch name, time)

- Languages

    - [`company`][company]: Autocompletion for most languages
    - [`flycheck`][flycheck]: Lint errors, warnings and notes

- Goodies

    - [`emmet-mode`][emmet-mode]: Add support for [Emmet][emmet] on HTML and CSS-like files
    - [`magit`][magit]: Git integration
    - [`multiple-cursors`][multiple-cursors]: Add support for multiple cursors
    - [`smartparens`][smartparens]: Supposedly smarter parentheses (still looking for better
      though)
    - [`helm`][helm]: Add an incremental selection framework to fuzzily narrow down choices
    - [`projectile`][projectile]: Add a project interface to issue actions within a project
      structure (eg. replace, search, grep)

And many more.

For more information about how stuff works, read the source Luke! For each package, I have left a
link to their website.

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
[zenburn]: http://kippura.org/zenburnpage/
[zenburn-theme]: https://github.com/bbatsov/zenburn-emacs

Installation
============

Emacs
-----

1. I use [Mitsuharu Yamamoto's Mac port][github-emacs]. The configuration files *should*
   work with any build nonetheless. In any case, install Emacs 24 or above.
1. Clone `dotemacs/` into `~/.emacs.d/`.
1. Edit `~/.emacs.d/lisp/init-constants.el` to suit your needs.

[github-emacs]: https://github.com/railwaycat/homebrew-emacsmacport

Flycheck
--------

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
