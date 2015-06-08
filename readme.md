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

Features
========

*Coming soon™*

Screenshots
===========

*Coming soon™*

Installation
============

Emacs
-----

1. I use [railwaycat's Mac port][railwaycat-emacs-releases]. The configuration files should work
   with any build, and OS X specific settings are enclosed in conditionnal clauses. In any case,
   install Emacs 24 or above.
1. Clone `dotamacs/` into `~/.emacs.d/`.

[railwaycat-emacs-realeases]: https://github.com/railwaycat/homebrew-emacsmacport/releases

Flycheck
--------

I use [flycheck][flycheck] as linter framework to display syntax warnings and errors whithin each
buffer. See below for the linters I personally use:

```bash
# Python 
pip install flake8


# SCSS
gem install scss_lint scss_lint_reporter_checkstyle
```

[flycheck]: https://github.com/flycheck/flycheck


