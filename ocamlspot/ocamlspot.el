; (***********************************************************************)
; (*                                                                     *)
; (*                            OCamlSpotter                             *)
; (*                                                                     *)
; (*                             Jun FURUSE                              *)
; (*                                                                     *)
; (*   Copyright 2008, 2009 Jun Furuse. All rights reserved.             *)
; (*   This file is distributed under the terms of the GNU Library       *)
; (*   General Public License, with the special exception on linking     *)
; (*   described in file LICENSE.                                        *)
; (*                                                                     *)
; (***********************************************************************)

; How-to-use
;
; Write the following to your .emacs



;; load-path
; (setq load-path (cons "WHERE-YOU-HAVE-INSTALLED-THE-ELISP" load-path))
;
;; set the path of the ocamlspot binary
;; this can be a shell command, e.g., "ocamlfind ocamlspot"
; (setq ocamlspot-command "WHERE-YOU-HAVE-INSTALLED-THE-BINARIES/ocamlspot")
;
; (require 'ocamlspot)
;
;; tuareg mode hook (use caml-mode-hook instead if you use caml-mode)
;   (add-hook 'tuareg-mode-hook
;         '(lambda ()
;            (local-set-key "\C-c;" 'ocamlspot-query)
; 	     (local-set-key "\C-c:" 'ocamlspot-query-interface)
;            (local-set-key "\C-c'" 'ocamlspot-query-uses)
;            (local-set-key "\C-c\C-t" 'ocamlspot-type)
;            (local-set-key "\C-c\C-i" 'ocamlspot-xtype)
;            (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
;            (local-set-key "\C-c\C-u" 'ocamlspot-use)
;            (local-set-key "\C-ct" 'caml-types-show-type)))
;
;; You can also change overlay colors as follows:
; (set-face-background 'ocamlspot-spot-face "#660000")
; (set-face-background 'ocamlspot-tree-face "#006600")



; ocamlspot-query
;   Show the type of the inner-most subexpression under the cursor.
;   If there is an identifier under the cursor, browse and show its definition
;
; ocamlspot-query-interface
;   Same as ocamlspot-query but browse identifier's interface rather than its defintion
;   This is currently under construction and does not work properly.
;
; ocamlspot-type
;   Show the type of the inner-most subexpression under the cursor.
;
; ocamlspot-type-and-copy
;   Same as ocamlspot-type but it also copies the type expression to the kill buffer.
;
; ocamlspot-use
;   Show the use information of the identifier under the cursor.
;
; ocamlspot-xtype
;   Same as ocamlspot-type but it shows you more detailed information: 
;   path id numbers. You can browse path names in this result using C-c;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Configurable variables

(eval-when-compile (require 'cl)) ; for `destructuring-bind'

(defgroup ocamlspot ()
  "OCamlSpotter: find the definition and type of variables."
  :group 'languages)

(defcustom ocamlspot-command "OCAML-SOURCE-TREE/ocamlspot/ocamlspot"
  "*The command which invokes ocamlspot."
  :type 'string :group 'ocamlspot)

(defcustom ocamlspot-debug nil
  "*Turn on ocamlspot debug output."
  :type 'boolean :group 'ocamlspot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constants

(defconst ocamlspot-process-buffer "*ocamlspot-process*"
  "The name of ocamlspot communication buffer")

(defconst ocamlspot-debug-buffer "*ocamlspot-debug*"
  "The name of ocamlspot debugging buffer")

(defconst ocamlspot-message-buffer "*ocamlspot-message*"
  "The name of ocamlspot message buffer")

(defconst ocamlspot-type-buffer "*ocamlspot-type*"
  "The name of ocamlspot type buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ocamlspot-path

; ocamlspot-path is superceded by ocamlspot-command, but if it exists,
; it overrides ocamlspot-command

(defun ocamlspot-get-command ()
  (if (boundp 'ocamlspot-path) ocamlspot-path ocamlspot-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; column chars => column bytes

; This looks complicated, but we need this conversion for multi-byte characters

(defun ocamlspot-string-of-line-to-point ()
  (buffer-substring-no-properties
   (line-beginning-position) (point)))

(defun ocamlspot-bytes-of-line-to-point ()
  (length
   (encode-coding-string
    (ocamlspot-string-of-line-to-point) buffer-file-coding-system)))

; It count one line less when the cursor is at (point-max) 
; and it is at the top of the line.
(defun ocamlspot-lines-of-point ()
  (count-lines (point-min) (min (1+ (point)) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; column bytes => column chars

; This looks complicated, but we need this conversion for multi-byte characters

; goto-line set mark and we see the result in the minibuffer
(defun ocamlspot-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; get the string at line
(defun ocamlspot-buffer-substring-at-line (line)
  ; no need of save-excursion
  (ocamlspot-goto-line line)
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun ocamlspot-chars-of-bytes-of-string (str bytes)
  (length
   (decode-coding-string
    (substring (encode-coding-string str buffer-file-coding-system)
               0 bytes)
    buffer-file-coding-system)))

(defun ocamlspot-pos-of-bytes-at-line (line bytes)
  ; no need of save-excursion
  (ocamlspot-goto-line line)
  (let ((pos-at-beginning-of-line (line-beginning-position))
        (chars-from-beginning-of-line
         (ocamlspot-chars-of-bytes-of-string
          (ocamlspot-buffer-substring-at-line line) bytes)))
    (+ pos-at-beginning-of-line chars-from-beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; location parser

; parses lxxxcxxxbxxx and returns the triplet
(defun ocamlspot-parse-location (s)
  (if (string-match "^l\\([\-0-9]+\\)c\\([\-0-9]+\\)b\\([\-0-9]+\\)$" s)
      (let ((line (string-to-number (match-string 1 s)))
            (colbytes (string-to-number (match-string 2 s)))
            (bytes (string-to-number (match-string 3 s))))
        (list line colbytes bytes))))

(defun ocamlspot-pos-of-location (buffer s)
  (destructuring-bind (line colbytes bytes) (ocamlspot-parse-location s)
    (if (= line -1) bytes
      (with-current-buffer buffer
        (ocamlspot-pos-of-bytes-at-line line colbytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Messaging

(setq ocamlspot-message-file-name nil) ;; only used for xtype

(defun ocamlspot-message-init (context-file-name)
  (setq ocamlspot-message-file-name context-file-name)
  (with-current-buffer (get-buffer-create ocamlspot-message-buffer)
    (erase-buffer)))

(defun ocamlspot-message-add (mes)
  (with-current-buffer (get-buffer-create ocamlspot-message-buffer)
    (if (/= 0 (current-column))
        (insert "\n"))
    (insert mes)))

; Display message in the echo area if it is enough short, then return the string.
; If too large, pop a buffer of the message if may-pop is t and return the buffer.
; Otherwise, returns nil
(defun ocamlspot-message-display (&optional may-pop)
  (with-current-buffer (get-buffer-create ocamlspot-message-buffer)
    (let ((lines ; how many lines in minibuffer-window ? 
           (count-screen-lines nil nil nil (minibuffer-window)))
          (max-echo-height 
           (if resize-mini-windows
               (cond ((floatp max-mini-window-height)
                      (* (frame-height) max-mini-window-height))
                     ((integerp max-mini-window-height)
                      max-mini-window-height)
                     (t 1)))))

      (if (or (<= lines  1)
              (<= lines max-echo-height))
          (progn
            (let ((mes (buffer-string)))
              (message mes)
              mes))
        (if may-pop ; buffer layout may change... no way to recover ?
            (progn
              (display-buffer ocamlspot-message-buffer)
              ocamlspot-message-buffer)
          ;; display the first max-echo-height lines
          (let ((lines (max 1 (1- max-echo-height))))
            (goto-char (point-min))
            (forward-visible-line (max 1 (- max-echo-height 2)))
            (message (concat (buffer-substring (point-min) (point)) "... Result is too long. Truncated."))
            nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; type buffer

(defun ocamlspot-type-init ()
  (with-current-buffer (get-buffer-create ocamlspot-type-buffer)
    (erase-buffer)
    (ocamlspot-xtype-mode t)))

(defun ocamlspot-type-add (mes)
  (with-current-buffer (get-buffer-create ocamlspot-type-buffer)
    (if (/= 0 (current-column))
        (insert "\n"))
    (insert mes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Overlays

;; the spot overlay
(defvar ocamlspot-spot-overlay (make-overlay 1 1))
(defface ocamlspot-spot-face
    '((t (:foreground "#88FF44")))
  "Face for ocamlspot spot highlight"
  :group 'ocamlspot)
(overlay-put ocamlspot-spot-overlay 'face 'ocamlspot-spot-face)

;; the tree overlay
(defvar ocamlspot-tree-overlay (make-overlay 1 1))
(defface ocamlspot-tree-face
    '((t (:foreground "#FF88FF")))
  "Face for ocamlspot tree highlight"
  :group 'ocamlspot)
(overlay-put ocamlspot-tree-overlay 'face 'ocamlspot-tree-face)

(defun ocamlspot-delete-overlays-now ()
  (interactive)
  (delete-overlay ocamlspot-tree-overlay)
  (delete-overlay ocamlspot-spot-overlay))

(defun ocamlspot-delete-overlays ()
  (unwind-protect
      (sit-for 10)
    (ocamlspot-delete-overlays-now)))

(defun ocamlspot-display-overlay (buffer position overlay)
  (if (string-match "^\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+\\):\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+\\)$" position)
      (let ((start (match-string 1 position))
            (end   (match-string 2 position)))
        (let ((start (ocamlspot-pos-of-location buffer start))
              (end   (ocamlspot-pos-of-location buffer end)))
          ;; display the result
          (set-buffer buffer)
          (goto-char start)
          (move-overlay overlay start end buffer)))
    ; this should be all
    (display-buffer buffer)
    (move-overlay overlay (point-min) (point-max) buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Warnings

(defun ocamlspot-warning ()
  (and (re-search-forward "^\\(Warning: .*\\)$" nil t)
       (match-string 1)))

(defun ocamlspot-warnings-rev (lst)
  (let ((warning (ocamlspot-warning)))
    (if warning (ocamlspot-warnings-rev (concat lst warning "\n"))
      lst)))

(defun ocamlspot-warnings ()
  (goto-char (point-min))
  (ocamlspot-warnings-rev ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; File access

(defun ocamlspot-find-file-existing (path)
  (if (file-exists-p path)
      (find-file-other-window path)
    (ocamlspot-message-add (format "ERROR: source file %s was not found" path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Queries

(defun ocamlspot-run-query (args &optional chdir)
  (with-current-buffer (get-buffer-create ocamlspot-process-buffer)
    (ocamlspot-process-mode t)
    (erase-buffer)
    (let ((command (concat (ocamlspot-get-command) " " args)))
      (insert command)
      (insert "\n")
      ;; chdir is required
      (if chdir (cd chdir))
      (let ((args (if ocamlspot-debug (concat "-debug " args) args)))
	(call-process shell-file-name nil t nil shell-command-switch
		      command)))))

(defun ocamlspot-query-string-at-cursor ()
  (format "%s:l%dc%d"
	  (buffer-file-name)
	  (ocamlspot-lines-of-point)
	  (ocamlspot-bytes-of-line-to-point)))

; launch ocamlspot, using the position of the cursor
; result is stored in the buffer "ocamlspot-process-buffer"
; the current buffer is stored in source-buffer
(defun ocamlspot-query-at-cursor (pre_extra_args &optional post_extra_args)
  ;; arguments
  (let ((file-name (buffer-file-name))
	(arg (ocamlspot-query-string-at-cursor))
	(post_sep (if post_extra_args " " "")))
    (ocamlspot-run-query (concat pre_extra_args " " arg post_sep post_extra_args) 
			 (file-name-directory file-name))))

;;; Search ocamlspot-process-buffer and return the first line which matches with ^<pattern>: "
(defun ocamlspot-find-query-result (pattern &optional to-kill)
  (set-buffer (get-buffer-create ocamlspot-process-buffer))
  (goto-char (point-min))
  (if (re-search-forward (concat "^" pattern ": \\(.*\\(\n +.*\\)*\\)") nil t)
      (let ((the-match (match-string 1)))
        (if to-kill (kill-new the-match))
        the-match)))

;; Scan the ocamlspot process output and search Tree.
;; If there is a Tree result, highlight it and returns t
;; Otherwise returns nil.
(defun ocamlspot-find-tree ()
  (save-excursion
    (let ((source-buffer (current-buffer)))
      (with-current-buffer (get-buffer-create ocamlspot-process-buffer)
	;; search the found tree element
	(let ((tree (ocamlspot-find-query-result "Tree")))
	  (if tree 
	      (if (string-match "^\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+:l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+:[0-9]+\\)$" tree)
		  (let ((pos (match-string 1 tree)))
		    ;; display the result
		    (save-current-buffer
		      (ocamlspot-display-overlay source-buffer pos ocamlspot-tree-overlay))
		    (ocamlspot-message-add (ocamlspot-warnings))
		    t))
	    (let ((err (ocamlspot-find-query-result "Error")))
		(if err
		    (ocamlspot-message-add (concat "Error: " err))
		  (ocamlspot-message-add "Error: no tree node found there")))
	    nil
	  ))))))

(defun ocamlspot-jump-to-spot (filename position)
  (if (string-match "\.cm[ioxa]$" filename)
      ;; It is not an .ml or .mli. Packed module.
      ;; CR jfuruse: opening a binary file is not good
      (ocamlspot-message-add "Packed module: %s" filename)
    (ocamlspot-display-overlay
     (ocamlspot-find-file-existing filename)
     position ocamlspot-spot-overlay)))

(defun ocamlspot-jump-to-path-range (path-range)
  (if (string-match "^<?\\(.*\\):\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+:l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+:[0-9]+\\|all\\|-1:-1\\)>?$" path-range)
      (let ((filename (match-string 1 path-range))
	    (position (match-string 2 path-range)))
	;; display the result
	(ocamlspot-jump-to-spot filename position)
	(let ((type (ocamlspot-find-val-or-type)))
	  ;; (if type (ocamlspot-message-add (format "Type: %s" type)))
	  ))
    ;; failed to get the normal result
    ;; CR jfuruse: this is an error message. Should be decolated?
    (ocamlspot-message-add path-range)))

(defun ocamlspot-find-type (&optional to-kill)
  (let ((type (ocamlspot-find-query-result "Type" to-kill)))
    (if type 
	(progn
	  (ocamlspot-message-add (format "Type: %s" type))
	  (ocamlspot-type-add (format "Type: %s" type))
	  type)
      (ocamlspot-message-add "no type found here")
      nil)))

;; same as type-in-buffer but for XType
(defun ocamlspot-find-xtype ()
  (let ((type (ocamlspot-find-query-result "XType")))
    (if type
	(progn
	  (ocamlspot-message-add (format "(* %s *)\n" ocamlspot-message-file-name))
	  (ocamlspot-message-add (format "%s" type))
	  (ocamlspot-type-add (format "(* %s *)\n" ocamlspot-message-file-name))
	  (ocamlspot-type-add (format "%s" type))
	  type)
      (ocamlspot-message-add "no type found here")
      nil)))

(defun ocamlspot-find-val-or-type (&optional to-kill)
  (let ((type (ocamlspot-find-query-result "Val" to-kill)))
    (if type
	(progn 
	  (ocamlspot-message-add (format "Val: %s" type))
	  (ocamlspot-type-add (format "Val: %s" type))
	  type)
      (ocamlspot-find-type to-kill))))

(defun ocamlspot-find-use ()
  (let ((use (ocamlspot-find-query-result "Use")))
    (if use
	(progn 
	  (ocamlspot-message-add (format "Use: %s" use))
	  use)
      (ocamlspot-message-add "no use information found here")
      nil)))

(defun ocamlspot-find-spot ()
  (let ((spot (ocamlspot-find-query-result "Spot")))
    (if spot (ocamlspot-jump-to-path-range spot)
      ;; no Spot:
      (let ((err (ocamlspot-find-query-result "Error")))
	(if err
	    (ocamlspot-message-add (concat "Error: " err))
	  ;; display debug info
	  (ocamlspot-message-add "No definition info found")
	  (ocamlspot-find-val-or-type)
	)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactives

(defun ocamlspot-wait (&optional may-pop)
  (ocamlspot-message-display may-pop)
  (ocamlspot-delete-overlays))

(defun ocamlspot-query (&optional args)
  (interactive)
  (let ((sel-window (selected-window)))
  (save-selected-window
    (ocamlspot-message-init (buffer-file-name))
    (ocamlspot-type-init)
    (ocamlspot-delete-overlays-now)
    (ocamlspot-query-at-cursor args)
    (when (ocamlspot-find-tree)
      ;; search the result
      (ocamlspot-find-spot))
    (ocamlspot-wait))
  ;; I dunno why but we need the following line to list-buffers work nicely
  (select-window sel-window)))

(defun ocamlspot-query-interface ()
  (interactive)
  (ocamlspot-query "--interface"))

(defun ocamlspot-type (&optional to-kill)
  (interactive)
  (ocamlspot-message-init (buffer-file-name))
  (ocamlspot-type-init)
  (ocamlspot-delete-overlays-now)
  (ocamlspot-query-at-cursor "-n")  
  (if (ocamlspot-find-tree)
      (save-current-buffer
        (ocamlspot-find-val-or-type to-kill)))
  (ocamlspot-wait t))

(defun ocamlspot-type-and-copy ()
  (interactive)
  (ocamlspot-type t))

(defun ocamlspot-xtype (&optional to-kill)
  (interactive)
  (ocamlspot-message-init (buffer-file-name))
  (ocamlspot-type-init)
  (ocamlspot-delete-overlays-now)
  (ocamlspot-query-at-cursor "-n")
  (if (ocamlspot-find-tree)
      (save-current-buffer
        (ocamlspot-find-xtype)))
  (display-buffer ocamlspot-type-buffer))

; CR can be shared with ocamlspot-type
(defun ocamlspot-use ()
  (interactive)
  (ocamlspot-message-init (buffer-file-name))
  (ocamlspot-type-init)
  (ocamlspot-delete-overlays-now)
  (ocamlspot-query-at-cursor "-n")
  (if (ocamlspot-find-tree)
      (save-current-buffer
        (ocamlspot-find-use)))
  (ocamlspot-wait t))

; CR can be shared with ocamlspot-type
(defun ocamlspot-query-uses ()
  (interactive)
  (let ((dir (read-directory-name "Search directory: "
				  (file-name-directory (buffer-file-name)))))
    (ocamlspot-message-init (buffer-file-name))
    (ocamlspot-type-init)
    (ocamlspot-delete-overlays-now)
    (ocamlspot-query-at-cursor "use" dir)
    (if (ocamlspot-find-tree)
	(progn
	 (ocamlspot-find-spot)
	 (display-buffer ocamlspot-process-buffer)
	 (ocamlspot-find-use)))
    (ocamlspot-wait t)))

;; Browsing of path-range <file:lxcxbx:lxcxbx>
(defun ocamlspot-beginning-of-path-range ()
  (search-backward "<"
		   (save-excursion (beginning-of-line) (point))))
(defun ocamlspot-end-of-path-range ()
  (search-forward ">"
		  (save-excursion (end-of-line) (point))))
(put 'ocamlspot-path-range 'beginning-op 'ocamlspot-beginning-of-path-range)
(put 'ocamlspot-path-range 'end-op 'ocamlspot-end-of-path-range)

(defun ocamlspot-path-range-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'ocamlspot-path-range)))
      (if bounds
	  (progn
	    (move-overlay ocamlspot-tree-overlay (car bounds) (cdr bounds))
	    (buffer-substring (car bounds) (cdr bounds))))))

(defun ocamlspot-browse-path-range-at-point ()
  (interactive)
  (let ((path-range (ocamlspot-path-range-at-point)))
    (if path-range
	(ocamlspot-jump-to-path-range path-range)
	(message "no path-range at point")
	)))

;; build query string for the cursor point
(defun ocamlspot-xtype-build-query-at-cursor ()
  (let ((path-name 
	 (save-excursion
	   (let ((end
;; Preferable, but not working correctly yet for A.B<cursor>.t
;;		  (if (skip-chars-forward "A-z0-9_)")
;;		      (point)
;;		    nil))
		  (if (skip-chars-forward "A-z0-9_().") ;; less powerful
		      (point)
		    nil))
		 (start
		  (if (skip-chars-backward "A-z0-9_().")
		      (point)
		    nil)))
	     (if (and start end) 
		 (progn
		   (move-overlay ocamlspot-tree-overlay start end)
		   (buffer-substring-no-properties start end)))))))
    (message (concat "path-name " path-name))
    (let ((file-name 
	   (save-excursion
	     (goto-char (point-min))
	     (if (re-search-forward "^(\\* \\(.*\\) \\*)$" nil t)
		 (match-string 1)))))
      (if (and 
	   file-name 
	   path-name 
	   (not (string= file-name "")) 
	   (not (string= path-name "")))
	  (concat file-name ":t:" path-name)))))

(defun ocamlspot-xtype-query ()
  (interactive)
  (let ((sel-window (selected-window)))
    (save-selected-window
      (ocamlspot-message-init (buffer-file-name))
      ;; (ocamlspot-type-init) ; We must keep xtype buffer
      (ocamlspot-delete-overlays-now)
      (let ((query (ocamlspot-xtype-build-query-at-cursor)))
	(if query
	    (progn
	      (message query)
	      (ocamlspot-run-query query)
	      (ocamlspot-find-spot)
	      (ocamlspot-wait))
	  (message "query empty"))))
    ;; I dunno why but we need the following line to list-buffers work nicely
    (select-window sel-window)))

(defun ocamlspot-xtype-mode-map ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c;" 'ocamlspot-xtype-query)
    keymap))

(defun ocamlspot-process-mode-map ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c;" 'ocamlspot-browse-path-range-at-point)
    keymap))

(define-minor-mode ocamlspot-xtype-mode
  "OCamlSpot XType mode."
  :lighter " OCamlSpot-XType"
  :keymap (ocamlspot-xtype-mode-map))

(define-minor-mode ocamlspot-process-mode
  "OCamlSpot Process mode."
  :lighter " OCamlSpot-Process"
  :keymap (ocamlspot-process-mode-map))

(provide 'ocamlspot)
