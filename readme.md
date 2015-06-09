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

  - Non-aggressive palette and pastel tones with [Zenburn][zenburn]
  - Prettify the mode line: less useless information, more room for actual
    information

- Languages

  - IPython integration
  - Autocompletion in most languages (still wondering which backend to use with
    Python)
  - Use linters for Python and Sass

- Web

  - Support for HTML, Sass, Less, JavaScript, AngularJS and JSON files
  - [Emmet][emmet] support for HTML, CSS-like files

- Goodies

  - Git integration
  - Support for multiple cursors
  - Parentheses are now smarter than you
  - Add a incremental selection framework to fuzzily narrow down choices
  - Add semantic breadcrumbs in the header line
  - Add a project interface to issue actions within a project structure (eg.
    replace, search, grep)

For more information about how stuff work, read the source Luke! For each
package, I have left a link to their website.

[zenburn]: http://kippura.org/zenburnpage/
[emmet]: http://emmet.io/

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
