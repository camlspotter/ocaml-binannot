#!/bin/sh

# This script checks that the patch extends ocaml compiler conservatively:
# Object files created by the original and patched compilers are the same.

set -e
rm -rf verify
svn checkout http://caml.inria.fr/svn/ocaml/release/3.12.0 verify/
cd verify
svn update -r 10643
./configure
make clean core coreboot world opt opt.opt
rm -rf boot/Saved _build/boot/Saved
find . -iregex '.*\.\(cm.*\|o\)' | sort | xargs md5sum > MD5-original
rm -rf boot/*
cp ../boot/[a-z]* boot
make clean world opt opt.opt
rm -rf boot/Saved _build/boot/Saved
find . -iregex '.*\.\(cm.*\|o\)' | sort | xargs md5sum > MD5-ocamlspot
if cmp MD5-original MD5-ocamlspot; then
    echo "All the object files are equal!"
else
    echo "Something different is created by compiler+ocamlspot!"
fi
 
