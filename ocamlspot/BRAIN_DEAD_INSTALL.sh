#!/bin/sh
# No warranty, no question. Just for brain-dead people.
set -e
svn checkout http://caml.inria.fr/svn/ocaml/release/3.12.0 .
svn update -r 10643
patch -p1 < compiler_patch.diff
./configure
make core coreboot
./build/mixed-boot.sh
cp boot/myocamlbuild boot/myocamlbuild.boot
make world opt opt.opt
echo ===================== testing =======================
(cd ocamlspot/tests; make; ./auto-test.pl  *.ml *.mli) 
echo ===================== congrats ======================
echo Now you can type make intall
echo Do not forget to install the elisp file, ocamlspot/ocamlspot.el
echo and configure your .emacs
