;; for caml-help.el
(defalias 'caml-info-other-window 'info-other-window)

;; for caml-types.el

(defalias 'caml-line-beginning-position 'line-beginning-position)

(defalias 'caml-read-event 'read-event)
(defalias 'caml-window-edges 'window-edges)
(defun caml-mouse-vertical-position ()
  (cddr (mouse-position)))
(defalias 'caml-ignore-event-p 'integer-or-marker-p)
(defalias 'caml-mouse-movement-p 'mouse-movement-p)
(defalias 'caml-sit-for 'sit-for)

(defmacro caml-track-mouse (&rest body) (cons 'track-mouse body))

(defun caml-event-window (e) (posn-window (event-start e)))
(defun caml-event-point-start (e) (posn-point (event-start e)))
(defun caml-event-point-end (e) (posn-point (event-end e)))

(defun caml-release-event-p (original event)
  (and (equal (event-basic-type original) (event-basic-type event))
       (let ((modifiers  (event-modifiers event)))
         (or (member 'drag modifiers)
             (member 'click modifiers)))))


(provide 'caml-emacs)
