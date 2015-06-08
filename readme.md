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

- Non-aggressive palette and pastel tones with Zenburn
- Autocompletion in most major modes (still wondering which backend to use with
  Python)
- Support for Web oriented technologies: Dockerfile, HTML, JavaScript,
  AngularJS, SCSS, JSON, Markdown, Python
- Emmet support for HTML, CSS, Less and Sass files
- Use linters for Python and Sass
- Git integration
- Add a pretty selecting framework to fuzzily narrow down choices
- Prettify the mode line: less useless information, more room for actual information
- Support multiple cursors
- Parentheses are now smarter than you
- Add a project interface to issue actions within a project structure (eg.
  replace, search, grep)
- IPython integration
- Add semantic breadcrumbs in the header line
- Any many more goodies

For more information about how stuff work, read the source Luke! For each
package, I have left links to their website.

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
