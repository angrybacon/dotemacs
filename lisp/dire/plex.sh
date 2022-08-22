#!/usr/bin/sh

find ~/Movies -type d \! -perm 775 -exec chmod 775 {} \; -print
find ~/Movies -type f \! -perm 664 -exec chmod 664 {} \; -print
