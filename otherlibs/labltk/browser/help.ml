let text = "                          OCamlBrowser Help

USE
   OCamlBrowser is composed of three tools, the Editor, which allows
   one to edit/typecheck/analyse .mli and .ml files, the Viewer, to
   walk around compiled modules, and the Shell, to run an OCaml
   subshell. You may only have one instance of Editor and Viewer, but
   you may use several subshells.

   As with the compiler, you may specify a different path for the
   standard library by setting OCAMLDIR. You may also extend the
   initial load path (only standard library by default) by using the
   -I command line option.

1) Viewer
   It displays the list of modules in the load path. Click on one to
   start your trip.

   The entry line at the bottom allows one to search for an identifier
   in all modules, either by its name (? and * patterns allowed) or by
   its type (if there is an arrow in the input). When search by type
   is used, it is done in inclusion mode (cf. Modules - search symbol)

   The Close all button is there to dismiss the windows created
   during your trip (every click creates one...) By double-clicking on
   it you will quit the browser.

   File - Open and File - Editor give access to the editor.

   File - Shell opens an OCaml shell.

   Modules - Path editor changes the load path.
        Pressing [Add to path] or Insert key adds selected directories
        to the load path.
        Pressing [Remove from path] or Delete key removes selected
        paths from the load path.
   Modules - Reset cache rescans the load path and resets the module
   cache. Do it if you recompile some interface, or change the load
   path in a conflictual way.

   Modules - Search symbol allows to search a symbol either by its
   name, like the bottom line of the viewer, or, more interestingly,
   by its type. Exact type searches for a type with exactly the same
   information as the pattern (variables match only variables),
   included type allows to give only partial information: the actual
   type may take more arguments and return more results, and variables
   in the pattern match anything. In both cases, argument and tuple
   order is irrelevant (*), and unlabeled arguments in the pattern
   match any label.

   (*) To avoid combinatorial explosion of the search space, optional
   arguments in the actual type are ignored if (1) there are to many
   of them, and (2) they do not appear explicitly in the pattern.

2) Module walking
   Each module is displayed in its own window.

   At the top, a scrollable list of the defined identifiers. If you
   click on one, this will either create a new window (if this is a
   sub-module) or display the signature for this identifier below.

   Signatures are clickable. Double clicking with the left mouse
   button on an identifier in a signature brings you to its signature,
   inside its module box.
   A single click on the right button pops up a menu displaying the
   type declaration for the selected identifier. Its title, when
   selectable, also brings you to its signature.

   At the bottom, a series of buttons, depending on the context.
   * Show all displays the signature of the whole module.
   * Detach copies the currently displayed signature in a new window,
     to keep it.
   * Impl and Intf bring you to the implementation or interface of
     the currently displayed signature, if it is available.

   C-s opens a text search dialog for the displayed signature.

3) File editor
   You can edit files with it, but there is no auto-save nor undo at
   the moment. Otherwise you can use it as a browser, making
   occasional corrections.

   The Edit menu contains commands for jump (C-g), search (C-s), and
   sending the current selection to a sub-shell (M-x). For this last
   option, you may choose the shell via a dialog.

   Essential function are in the Compiler menu.

   Preferences opens a dialog to set internals of the editor and
   type checker.

   Lex (M-l) adds colors according to lexical categories.

   Typecheck (M-t) verifies typing, and memorizes it to let one see an
   expression's type by double-clicking on it. This is also valid for
   interfaces. If an error occurs, the part of the interface preceding
   the error is computed.

   After typechecking, pressing the right button pops up a menu giving
   the type of the pointed expression, and eventually allowing to
   follow some links.

   Clear errors dismisses type checker error messages and warnings.

   Signature shows the signature of the current file.

4) Shell
   When you create a shell, a dialog is presented to you, letting you
   choose which command you want to run, and the title of the shell
   (to choose it in the Editor).

   You may change the default command by setting the OLABL environment
   variable.

   The executed subshell is given the current load path.
   File: use a source file or load a bytecode file.
     You may also import the browser's path into the subprocess.
   History: M-p and M-n browse up and down.
   Signal: C-c interrupts and you can kill the subprocess.

BUGS

* When you quit the editor and some file was modified, a dialogue is
  displayed asking wether you want to really quit or not. But 1) if
  you quit directly from the viewer, there is no dialogue at all, and
  2) if you close from the window manager, the dialogue is displayed,
  but you cannot cancel the destruction... Beware.

* When you run it through xon, the shell hangs at the first error. But
  its ok if you start ocamlbrowser from a remote shell...

TODO

* Complete cross-references.

* Power up editor.

* Add support for the debugger.

* Make this a real programming environment, both for beginners an
  experimented users.


Bug reports and comments to <garrigue@kurims.kyoto-u.ac.jp>
";;
