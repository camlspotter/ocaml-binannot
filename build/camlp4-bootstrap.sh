#!/bin/sh
set -e
cd `dirname $0`/..

TMPTARGETS="\
  camlp4/boot/Lexer.ml"

TARGETS="\
  camlp4/boot/Camlp4.ml \
  camlp4/boot/camlp4boot.ml"

if [ -x ./boot/myocamlbuild.native ]; then
  OCAMLBUILD=./boot/myocamlbuild.native
else
  OCAMLBUILD="./boot/ocamlrun boot/myocamlbuild"
fi
$OCAMLBUILD $TMPTARGETS $TARGETS

for t in $TARGETS; do
  echo promote $t
  cp _build/$t $t
done
