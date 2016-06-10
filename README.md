DotEmacs
========

This repository contains my personal Emacs configuration.

Have fun tinkering : j

What the .el
============

If you are wondering where has gone all of the Lisp, fear not, for it is
automagically tangled from an Org file into a fresh Lisp file with the help of the
all powerful `org` package. This is called literate programming.

Head over to the actual configuration: [dotemacs.org][self.dotemacs].

Screenshots
===========

My Emacs configuration is a living mixture that I edit on a daily basis. As such, the
following screenshots might not reflect exactly what is on `:master`.

- [Helm][screenshots.helm]

More coming *soon* :tm:

Installation
============

I primarily use Emacs on OS X, but I trust your *google-fu* to find the equivalent for
the following commands to run on your setup.

Emacs
-----

1. I use [Yamamoto Mitsuharu's port][github.mitsuharu]. The configuration files
   *should* work with any build nonetheless. In any case, install Emacs 24 or above.
1. Clone `dotemacs/` as `~/.emacs.d/`.

Ag
--

My Helm setup uses `ag` instead of `grep` to lookup files.

```sh
brew install the_silver_searcher
```

Flycheck
--------

I use [Flycheck][github.flycheck] as linter framework to display syntax warnings and
errors whithin each buffer.

```sh
pip install flake8                                   # Python
gem install scss_lint scss_lint_reporter_checkstyle  # SCSS
```

Font Awesome
------------

There are special characters in the mode-line. Install [Font Awesome][font-awesome].

[font-awesome]: http://fontawesome.io/
[github.flycheck]: https://github.com/flycheck/flycheck
[github.mitsuharu]: https://github.com/railwaycat/homebrew-emacsmacport
[screenshots.helm]: https://drive.google.com/open?id=0BwTSOByd3qSFMmZqVHBpTlk2Q1E
[self.dotemacs]: ./dotemacs.org
