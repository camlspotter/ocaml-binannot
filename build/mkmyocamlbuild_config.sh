#!/bin/sh

cd `dirname $0`/..

sed \
    -e 's/^#ml \(.*\)/\1/' \
    -e 's/\\"/"/g' \
    -e 's/^\(#.*\)$/(* \1 *)/' \
    -e 's/^\(.*\$([0-9]).*\)$/(* \1 *)/' \
    -e 's/^\([^(=]*\)=\([^"]*\)$/let <:lower<\1>> = "\2";;/' \
    -e 's/\$(\([^)]*\))/"\^<:lower<\1>>\^"/g' \
    -e 's/""\^//g' \
    -e 's/\^""//g' \
    -e 's/^let <lower>\(MAKE\|DO\).*$//g' \
    -e 's/"true"/true/g' \
    -e 's/"false"/false/g' \
    config/Makefile | sed -f build/tolower.sed | sed -f build/tolower.sed > myocamlbuild_config.ml
