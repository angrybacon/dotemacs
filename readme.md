DotEmacs
========


This repository contains my personal Emacs configuration (and installed
packages). It was originally meant for personal uses and to help me keep my
Emacs files when moving onto another machine. I've kept this repository public
for other Emacs lovers.

Have fun tinkering : j


Notes
=====


This has been tested with railwaycat's [Emacs Mac port for Homebrew][railwaycat-emacs]
and should work with any build.

[railwaycat-emacs]: https://github.com/railwaycat/homebrew-emacsmacport


Screenshots
===========


*Coming soon™*


Features
========


Below are the most notable features supported:

- Theming

    - [`zenburn-theme`][zenburn-theme]: Non-aggressive palette and pastel tones with [Zenburn][zenburn]
    - [`smart-mode-line`][smart-mode-line]: Prettify the mode line: less useless information, more room for
      actual information

- Languages

    - [`company`][company]: Autocompletion in most languages (still testing different backend
      for Python)
    - [flycheck][flycheck]: Use linters for Python and Sass
    - IPython integration (work in progress)

- Web

    - [emmet][emmet]: Add Emmet support for HTML, CSS-like files
    - Support for HTML, Sass, Less, JavaScript, AngularJS and JSON files

- Goodies

    - [magit][magit]: Git integration
    - [mutliple-cursors][mutliple-cursors]: Support for multiple cursors
    - [smartparens][smartparens]: Parentheses are now smarter than you
    - [helm][helm]: Add a incremental selection framework to fuzzily narrow down choices
    - [projectile][projectile]: Add a project interface to issue actions within a project structure (eg.
      replace, search, grep)
    - Add semantic breadcrumbs in the header line

For more information about how stuff work, read the source Luke! For each
package, I have left a link to their website.

[company]: https://github.com/company-mode/company-mode
[emmet]: https://github.com/smihica/emmet-mode
[flycheck]: https://github.com/flycheck/flycheck
[helm]: https://github.com/emacs-helm/helm
[magit]: https://github.com/magit/magit
[mutliple-cursors]: https://github.com/magnars/multiple-cursors.el
[projectile]: https://github.com/bbatsov/projectile
[smart-mode-line]: https://github.com/Malabarba/smart-mode-line/
[smartparens]: https://github.com/Fuco1/smartparens
[zenburn]: http://kippura.org/zenburnpage/
[zenburn-theme]: https://github.com/bbatsov/zenburn-emacs


Installation
============


Emacs
-----


1. I use [railwaycat's Mac port][railwaycat-emacs-releases]. The configuration
   files should work with any build nonetheless. In any case, install Emacs 24
   or above.

1. Clone `dotemacs/` into `~/.emacs.d/`.

1. Edit `~/.emacs.d/lisp/init-constants.el` to suit your needs.

[railwaycat-emacs-releases]: https://github.com/railwaycat/homebrew-emacsmacport/releases


Flycheck
--------


I use [flycheck][flycheck] as linter framework to display syntax warnings and
errors whithin each buffer. See below for the linters I personally use:

```bash
# Python
pip install flake8

# SCSS
gem install scss_lint scss_lint_reporter_checkstyle
```

[flycheck]: https://github.com/flycheck/flycheck
