DotEmacs
========

This repository contains my personal Emacs configuration.

Have fun tinkering : j

What the .el
============

If you are wondering where has gone all of the Lisp, fear not, for it is
automagically tangled from an Org file into a fresh Lisp file with the help of the
all powerful `org` package. This is called literate programming.

Head over to the actual configuration: `dotemacs.org`.

Screenshots
===========

My Emacs configuration is a living mixture that I edit on a daily basis. As such, the
following screenshots might not reflect exactly what is on `:master`.

![dotemacs.2016-03-14.png](http://i.imgur.com/BN8dIS9.png)

More coming *soon*™

Installation
============

Emacs
-----

1. I use [Yamamoto Mitsuharu's port][github-mitsuharu]. The configuration files
   *should* work with any build nonetheless. In any case, install Emacs 24 or above.
1. Clone `dotemacs/` into `~/.emacs.d/`.
1. Edit `~/.emacs.d/dotemacs.org` (Bootstrap > Set constants) to suit your needs.

Ag
--

My Helm setup uses `ag` instead of `grep` to lookup files.

<!-- language: lang-sh -->

    brew install the_silver_searcher

Flycheck
--------

I use [`flycheck`][github-flycheck] as linter framework to display syntax warnings and
errors whithin each buffer.

<!-- language: lang-sh -->

    pip install flake8
    gem install scss_lint scss_lint_reporter_checkstyle

[github-flycheck]: https://github.com/flycheck/flycheck
[github-mitsuharu]: https://github.com/railwaycat/homebrew-emacsmacport
