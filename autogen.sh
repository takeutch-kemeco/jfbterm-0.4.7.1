#!/bin/bash

aclocal
automake --add-missing --copy --force-missing
autoconf
./configure $@

