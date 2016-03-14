DotEmacs
========


This repository contains my personal Emacs configuration. It was originally meant for personal uses and to help me keep my Emacs files when moving onto another machine. I've kept this repository public for other Emacs lovers.

Have fun tinkering : j


Notes
=====


This has been tested with [Yamamoto Mitsuharu's Mac port][yamamoto-emacs] and should work with any build.

[yamamoto-emacs]: https://github.com/railwaycat/homebrew-emacsmacport


Screenshots
===========

![dotemacs.2015-08-18.png](https://bitbucket.org/repo/noxGbL/images/1027694942-dotemacs.2015-08-18.png)

More coming *soon*™


Features
========


Below are the most notable features supported:

- Theming

    - [`zenburn-theme`][zenburn-theme]: Non-aggressive palette and pastel tones with
      [Zenburn][zenburn]
    - [`powerline`][powerline]: Prettify the mode line with a custom template: less useless
      information, more room for actual information

- Languages

    - [`company`][company]: Autocompletion in most languages (still testing different backend for
      Python)
    - [`flycheck`][flycheck]: Use linters for Python and Sass

- Web

    - [`emmet`][emmet]: Add Emmet support for HTML, CSS-like files
    - Support for HTML, Sass, Less, JavaScript, AngularJS and JSON files

- Goodies

    - [`magit`][magit]: Git integration
    - [`multiple-cursors`][multiple-cursors]: Support for multiple cursors
    - [`smartparens`][smartparens]: Parentheses are now smarter than you
    - [`helm`][helm]: Add a incremental selection framework to fuzzily narrow down choices
    - [`projectile`][projectile]: Add a project interface to issue actions within a project
      structure (eg. replace, search, grep)
    - Add semantic breadcrumbs in the header line

For more information about how stuff works, read the source Luke! For each package, I have left a
    link to their website.

[company]: https://github.com/company-mode/company-mode
[emmet]: https://github.com/smihica/emmet-mode
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


1. I use [Yamamoto Mitsuharu's Mac port][yamamoto-emacs-releases]. The configuration files should
   work with any build nonetheless. In any case, install Emacs 24 or above.

1. Clone `dotemacs/` into `~/.emacs.d/`.

1. Edit `~/.emacs.d/lisp/init-constants.el` to suit your needs.

[yamamoto-emacs-releases]: https://github.com/railwaycat/homebrew-emacsmacport/releases


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
to their documentation.

- `flake8`: `~/.flake8rc`
- `scss-lint`: `~/.scss-lint.yml`

[flycheck]: https://github.com/flycheck/flycheck