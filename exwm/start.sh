#!/bin/sh

# Set the display DPI
xrdb ./xresources

# Invoke Emacs
exec dbus-launch --exit-with-session emacs --debug-init --maximized
