OCaml Binary Annots
===================

The goal of this branch is to replace textual .annot files by
binary files, containing more information. In the long term, these
binary files should contain all the information useful for ocamlspot,
ocamlwizard, and other editor enhancer tools.

Current status
--------------

+ The parse tree has been improved with more location information
+ The typed tree has been improved with more location information, and
   is now a surjection of the parse tree
+ A new module Pprintast has been copied from BER-metaocaml, to print
   the parse tree to equivalent sources
+ The typed tree has been modified (replacement of Lazy values) to allow
   direct output in a file
+ A new module Untypeast has been created to convert a typed tree to an
   equivalent parse tree
+ Using the -annot generates .cmt and .cmti files, which contains a dump
   of the typed tree
+ A tool tools/genannot has been added, to convert .cmt files to .annot
   files

+ TODO: the typed tree to parse tree converter needs to replace Path.t with
   Longidents of the shortest possible size, to avoir conflicts
+ TODO: in case of a type error, we might want to still dump a complete typed tree with partial type information
+ TODO: in case of type error, there is no way to distinguish an erroneous .cmti
   file where just one signature was correctly typed from a correct .cmti.

Current format of .cmt/.cmti files
----------------------------------

Currently, the format of .cmt/.cmti files is a binary dump (output_value)
of a value of type 'Typedtree.saved_type array'. The array contains just one
value if no error happened.
