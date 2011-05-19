OCamlSpotter : OCaml source browsing
====================================

  Camel spotter: Oh, it was extremely interesting, very, very -
    quite... it was dull; dull, dull, dull, oh God it was dull. 
    Sitting in the Waterloo waiting room. ...

                                     from You're No Fun Anymore, 
                                    Monty Python's Flying Circus

OCaml module language is powerful. So extremely powerful that you can
be lost yourself easily inside a huge OCaml project with tons of
modules and functors. Spotting the definition of a variable is
sometimes "extremely interesting". Let's see such an example:

  include M
  open N
  open O
  let _ = f 1

Suppose you are not familiar with the function f and want to spot its
definition. Where to find it? Just from the code you are looking at,
it is not sure: if you are enough lucky it may be f in m.ml, n.ml or
o.ml in your current working directory. Otherwise, it may be O.f in
n.ml. Or probably N.O.f in m.ml. If you are unlucky and the project is
so sophisticated, there could be complex module operations in m.ml
(i.e includes, functor applications) and therefore the definition
might be found somewhere completely unexpected. Module packing, module
load paths and library installation without .ml files complicate the
situation even worse.

You first days after joining a large OCaml project should be this kind
of manual value definition spotting with find + grep commands. Yes,
actually it is very educational: you can learn a lot about the project
struggling in the source tree (only if your collegues have organized
things very well :-P), but it is still a dull job...

To say short, OCamlSpotter is a tool which does this dull job
automatically for you and permits your energy for something more
"interesting" like Yeti spotting:

 - The -annot option of ocamlc and ocamlopt is extended and creates 
   <module>.spot files (<module>.spit for .mli), which record the
   location information of the names defined and used in the module.

 - A small application ocamlspot provides automatic where-about
   spotting of the definition of the name you are interested in.

 - ocamlspot.el provides interactive ocaml-spotting of definition 
   locations in emacs.

 - Interfaces for other editors such as vi can be built easily, if you
   want. I do not want.
   
Spotting / Definition analysis
==============================

SYNOPSIS

      ocamlspot [search]
            search ::= [file] | [file]:[pos] | [file]:[kind]:[path]
            pos ::= l[line]c[column_bytes] | b[bytes] | [bytes]
            kind ::= v|t|e|m|mt|c|ct

DESCRIPTION

      Retrieve various annotation information from .spot or .spit files
      of the source code position or identifier, specified by the
      search spec. Information is prited to stdout. They are:

      Spot: file:pos
          The definition position of the object which the search spec
          points to, if available.

      Type: type
      XType: type     
          The type of the object which the search spec points to, if
          available. In XType, paths are printed with position
          informaiton: for example, "unit__6" instead of "unit".

SEARCH

      Three kinds of searches are available: file-name-only, 
      by-position and by-path:

      File-name-only search show the position independent information
      of the module specified by [file].

        Example:

	$ ocamlspot -i ocamlspot.ml
	Compile: ../ocamlc -nostdlib -I ../stdlib -annot -annot -w Ae -warn-error Ae -I ../parsing -I ../utils -I ../typing -I ../driver -I ../bytecomp -I ../tools -I ../toplevel/ -I ../otherlibs/unix -c ocamlspot.ml
	Included_dirs:
	  /..../ocaml/ocamlspot/
	  /..../ocaml/ocamlspot/../stdlib
	  /..../ocaml/ocamlspot/../parsing
	  /..../ocaml/ocamlspot/../utils
	  /..../ocaml/ocamlspot/../typing
	  /..../ocaml/ocamlspot/../driver
	  /..../ocaml/ocamlspot/../bytecomp
	  /..../ocaml/ocamlspot/../tools
	  /..../ocaml/ocamlspot/../toplevel/
	  /..../ocaml/ocamlspot/../otherlibs/unix
	BYE!

      By-position search has the form [file]:[pos]. [file] is the path
      of the target ml/mli file and [pos] is the position in the
      target file. [pos] has three formats: l[line]c[column_bytes] by the
      line number and column bytes, b[bytes] (and [bytes] for backward
      compatibility) by the bytes from the beginning of the source
      code. 

        Example:

	$ ocamlspot ocamlspot.ml:l129c14
	Tree: l129c8b4035:l129c28b4055           # pos of the inner most node
	In_module: With_pos.Fix
	Use: Type, Path__360.t__G
	Type: Path.t
	XType: Path__360.t__G                    # type with ident id numbers
	Spot: /..../ocaml/typing/path.ml:l15c5b875:l18c19b949 # the definition
	BYE!

      Note that bytes are not chars in multi-byte-character
      environment. OCaml is completely free from such mess and you
      might need to count chars by yourself.


      By-path search shows the definition spot of the given path 
      with ident id numbers. It will be useful with conjunction with 
      the path names obtained from XType: fields. No fancy elisp wrapper
      is not available for it, though.

        Example:

	# checking a type named Path__360.t__G
	$ ocamlspot ocamlspot.ml:t:Path__360.t__G  
	Spot: /..../ocaml/typing/path.ml:l15c5b875:l18c19b949
	BYE!

OPTIONS
  
      -n  
          Skips the spotting, which may takes time. Useful if you are
          interested in other things than the definition analysis,
          i.e. types, paths, etc.
    
Bugs and future works
=====================

This software contain lots of bugs and todos.

License
=======

Read LICENSE file in this directory. 

Author
======

  Jun FURUSE (jun.furuse@gmail.com). Bug reports and comments are
  welcome. Patches are appreciated. Donating camel related gadgets is
  the most wonderful. Complaints might be simply ignored.
