#!/bin/bash
cabal build
mkdir -p /tmp/installdir/usr/local/bin
cp dist/build/mallard/mallard /tmp/installdir/usr/local/bin/mallard
fpm -s dir -t deb -n mallard -v 0.6.3.0 -C /tmp/installdir \
        -d libpq5 \
        usr/local/bin/mallard