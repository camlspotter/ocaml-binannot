#!/bin/sh
set -e
export OCAMLBUILD_PARTIAL="true"
cd `dirname $0`/..
. build/targets.sh
set -x
$OCAMLBUILD $@ $OCAMLOPT_BYTE $OCAMLLEX_BYTE $OCAMLBUILD_NATIVE
