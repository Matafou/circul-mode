;;; circul.el --- Circulation over Buffers  -*- lexical-binding: t; -*-
;; Circulation over Buffers 
;;
;; Copyright (C) 2022 Pierre Courtieu
;;
;; Authors: Pierre Courtieu
;; Maintainer: Pierre Courtieu <Pierre.Courtieu@gmail.com>
;; URL: https://github.com/Matafou/circul-mode
;; Package-Requires: ((emacs "28.1"))
;; Version: ???
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License in file COPYING in this or one of the parent
;; directories for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with "prooftree". If not, see <http://www.gnu.org/licenses/>.
;;
;; Features that might be required by this library:
;;
;; only stock ones: registers, markers, overlays.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file mixes registers and an adhoc list of buffers into so
;; called "landmarks" to provide several features:
;;
;;  1) easy cycling between "interesting" buffers. Interestingness
;;    being governed by user defined regexps.
;;
;;  2) easy assignment of keybindings to jump to a landmark (either a
;;  buffer or a position). Positions are optionally made visible by a
;;  symbol on the fringe and/or a special face on the character next
;;  to the marker.
;;
;; By default this file does not set any keybinding, (an exemple of
;; key setting is given in the comments below). See below to do so.
;;
;; Implementation is very light: 1) uses the bury-buffer function to
;; do the work, 2) is a small wrapper over registers on markers + some
;; mechanics to make thoses register visible (using overlays).
;;
;;
;; TODO: Provide typical american keyboard configuration as an option?
;;
;;;;;;;;;;;;;;;;;; EXAMPLE OF SET-KEY ;;;;;;;;;;;;;;;;;;;;;
;; for cycling:
;; (global-set-key [(control kp-enter)]  'circul-bury-buffer)
;; (global-set-key [(control kp-add)]  'circul-unbury-buffer)
;; (global-set-key [(control meta mouse-4)]  'circul-bury-buffer)
;; (global-set-key [(control meta mouse-5)]  'circul-unbury-buffer)
;;
;; enable the keypad, whatever the state of verr. num. switch
;; (circul-assign ?0 [(control kp-0)] [(control shift kp-0)] [(kp-0)])
;; (circul-assign ?0 [(control kp-insert)] [(control shift kp-insert)] [(kp-insert)])
;; (circul-assign ?1 [(control kp-1)] [(control shift kp-1)] [(kp-1)])
;; (circul-assign ?1 [(control kp-end)] [(control shift kp-end)] [(kp-end)])
;; (circul-assign ?2 [(control kp-2)] [(control shift kp-2)] [(kp-2)])
;; (circul-assign ?2 [(control kp-down)] [(control shift kp-down)] [(kp-down)])
;; (circul-assign ?3 [(control kp-3)] [(control shift kp-3)] [(kp-3)])
;; (circul-assign ?3 [(control kp-next)] [(control shift kp-next)] [(kp-next)])
;; (circul-assign ?4 [(control kp-4)] [(control shift kp-4)] [(kp-4)])
;; (circul-assign ?4 [(control kp-left)] [(control shift kp-left)] [(kp-left)])
;; (circul-assign ?5 [(control kp-begin)] [(control shift kp-5)] [(kp-begin)])
;; (circul-assign ?5 [(control kp-5)] [(control shift kp-5)] [(kp-5)])
;; (circul-assign ?6 [(control kp-right)] [(control shift kp-right)] [(kp-right)])
;; (circul-assign ?7 [(control kp-7)] [(control shift kp-7)] [(kp-7)])
;; (circul-assign ?7 [(control kp-home)] [(control shift kp-home)] [(kp-home)])
;; (circul-assign ?8 [(control kp-8)] [(control shift kp-8)] [(kp-8)])
;; (circul-assign ?8 [(control kp-up)] [(control shift kp-up)] [(kp-up)])
;; (circul-assign ?9 [(control kp-9)] [(control shift kp-9)] [(kp-9)])
;; (circul-assign ?9 [(control kp-prior)] [(control shift kp-prior)] [(kp-prior)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defcustom circul-uninteresting-buffer-regexp
  "\\`\\*\\|\\` \\|.+\\.log\\'"
  "Regexp of unintersting buffer names."
  :type '(string)
  :group 'circul)

;;;###autoload
(defcustom circul-uninteresting-buffer-exception-regexp 
  ""
  "Regexp of exceptions for `circul-uninteresting-buffer-regexp'."
  :type '(string)
  :group 'circul)

;;;###autoload
(defcustom circul-show-landmark-fringe t  
  "If non-nil show circul-mode landmarks in the fringe.

Both `circul-show-landmark-fringe' and
`circul-show-landmark-position' can be non-nil."
  :type '(boolean)
  :group 'circul)

(defcustom circul-show-landmark-position t  
  "If non-nil show circul-mode landmarks with a special face.

Both `circul-show-landmark-fringe' and
`circul-show-landmark-position' can be non-nil. Changing this
variable only affects the future landmarks."
  :type '(boolean)
  :group 'circul)

(defvar circul-buffer-alist nil "List of buffers currently assigned to circul landmarks.")
(defvar circul-overlay-alist nil "List of overlays of circul landmarks.")


;;;###autoload
(defun circul-is-uninteresting-buffer (buf)
  "Return t if string BUF is not an interesting buffer name.
See `circul-uninteresting-buffer-regexp'
and `circul-uninteresting-buffer-exception-regexp'."
  (and (string-match circul-uninteresting-buffer-regexp buf)
       (not (string-match circul-uninteresting-buffer-exception-regexp buf))))

;;;###autoload
(defun circul-is-interesting-buffer (buf)
  "Return t if string BUF is an interesting buffer name.
See `circul-uninteresting-buffer-regexp'
and `circul-uninteresting-buffer-exception-regexp'."
  (not (circul-is-uninteresting-buffer buf)))

(defun circul--find-first-interesting (list-buf)
  "Return the first interesting buffer in LIST-BUF, nil if none exist."
  (buffer-name
   (car
    (cl-member-if (lambda (l) (circul-is-interesting-buffer (buffer-name l))) list-buf))))

(defun circul--find-last-interesting (list-buf)
  "Return the most buried interesting buffer in LIST-BUF, nil if none exist."
  (circul--find-first-interesting (reverse list-buf)))

;;;###autoload
(defun circul-bury-buffer ()
  "Bury current buffer and switch to:
- first interesting buffer in (buffer-list) if it exists
     - first buffer of (buffer-list) otherwise"
  (interactive)
  (bury-buffer)
  (let* ((intbuf (circul--find-first-interesting (buffer-list))))
    (if (eq intbuf nil) () (switch-to-buffer intbuf))))

;;;###autoload
(defun circul-unbury-buffer ()
  "Unbury the most buried interesting buffer if it exists.

Unbury the most buried non interesting buffer otherwise."
  (interactive)
  (let* ((intbuf (circul--find-last-interesting (buffer-list))))
    (if (eq intbuf nil) (unbury-buffer) (switch-to-buffer intbuf))))

(defun circul--add-assoc (k el l)
  "Add (K . EL) in the list L."
  (cons (cons k el) l))

;;; Assigning landmarks to buffers

;;;###autoload
(defun circul--unbury-landmark-buffer (n)
  "Unbury buffer landmark N."
  (let* ((buf (cdr (assoc n circul-buffer-alist))))
    (if (eq buf nil) (error "No buffer assign to this")
      (switch-to-buffer buf))))

;; Remove a killed buffer from the list
(add-hook
 'kill-buffer-hook 
 (lambda ()
   (let ((bufnme (buffer-name (current-buffer))))
     (setq circul-buffer-alist (rassq-delete-all bufnme circul-buffer-alist)))))

;;;###autoload
(defun circul-buf-to-landmark (n)
  "Assign landmark N to the current buffer.
Delete previous landmark N in the process."
  (interactive)
  (setq register-alist (assq-delete-all n register-alist))
  (setq circul-buffer-alist 
	(circul--add-assoc n (buffer-name (car (buffer-list))) 
		           circul-buffer-alist)))

;;;###autoload
(defface circul-landmark-face
  '((t (:box (:line-width (-1 . -1) :color "yellow")) ))
  "Used to mark register positions in a buffer."
  :group 'faces)

(defun circul--grow-overlay (ov beg end)
  "Grow an overlay by one char if possible, preferably backward.

BEG and END are the beginning and end of the overlay.
Argument OV is the overlay."
  (if (not (eq (point-min) beg)) (move-overlay ov (- beg 1) end)
    (if (not (eq (point-max) end)) (move-overlay ov beg (+ 1 end)))))

;;;###autoload
(defun circul-pos-to-landmark (n)
  "Assign landmark N to the current position.
Delete previous landmark N in the process."
  (interactive)
  (setq circul-buffer-alist (assq-delete-all n circul-buffer-alist))
  (let ((oldov (cdr (assq n circul-overlay-alist)))
        (m (point-marker)))
    (and oldov (delete-overlay oldov))
    ;; this make the register and overlay move accordingly
    ;; point-to-register does not seem to allow this
    (set-marker-insertion-type m t)
    (set-register n m)
    (let ((sovl (make-overlay (point)(+ 1 (point)) nil t nil)))
      (and circul-show-landmark-position
           (overlay-put sovl 'face 'circul-landmark-face))
      (overlay-put sovl 'help-echo (concat "Register: `" (char-to-string ?n) "'"))
      (and circul-show-landmark-fringe
           (overlay-put sovl 'before-string (propertize "A" 'display '(left-fringe right-triangle))))
      ;; The overlay  should remain only one char long i possible.
      (overlay-put
       sovl 'modification-hooks
       (list (lambda (ov x beg end &optional lgthbefore)
               (if (and x (> lgthbefore 0))
                   (circul--grow-overlay ov beg end)))))
      (setq circul-overlay-alist (circul--add-assoc n sovl circul-overlay-alist)))))

;;;###autoload
(defun circul-jump-to-landmark (n)
  "Jump to the register or the buffer named N."
  (interactive)
  (if (cdr (assoc n circul-buffer-alist)) (circul--unbury-landmark-buffer n)
    (jump-to-register n)))

;; An all-in-one setter, typical use:
;; (circul-assign-register-to-key ?0
;;    [(control kp-insert)] [(control shift kp-0)] [(kp-insert)])
;;;###autoload
(defun circul-assign-keys-to-landmark (r kpos kbuf kgoto)
  "Sets keybindings related to a same landmark R.

Main `circul-mode' key binding function. A landmark is either a
buffer or a position in a buffer associated to a character (like
registers). This function assigns keybindings to three usual
commands acting on landmark R:

KPOS: assign current position to R (`circul-pos-to-landmark').
KBUF: assign current buffer to R (`circul-buf-to-landmark').
KGOTO: jump to R (`circul-jump-to-landmark').
ex:
 (circul-assign-keys-to-landmark ?0 [(control f5)] [(control shift f5)] [(f5)])
 (circul-assign-keys-to-landmark
    ?1 [(control kp-insert)] [(control shift kp-0)] [(kp-insert)])"
  (global-set-key kpos  `(lambda () (interactive) (circul-buf-to-landmark ,r)))
  (global-set-key kbuf  `(lambda () (interactive) (circul-pos-to-landmark ,r)))
  (global-set-key kgoto `(lambda () (interactive) (circul-jump-to-landmark ,r))))


(defun circul-assign-keypad-config ()
  "Bind keypad number keys to circul-mode in the standard way.

More precisely it calls `circul-assign-keys-to-landmark' for each keypad numeric
key.

Warning: this tries to intercept all versions of keypad number
keys, whatever the state of the verr num switch. See
`circul-assign-keys-to-landmark' for more fine grained keybindings."
  (interactive)
  (circul-assign-keys-to-landmark ?0 [(control kp-0)] [(control shift kp-0)] [(kp-0)])
  (circul-assign-keys-to-landmark ?0 [(control kp-insert)] [(control shift kp-insert)] [(kp-insert)])
  (circul-assign-keys-to-landmark ?1 [(control kp-1)] [(control shift kp-1)] [(kp-1)])
  (circul-assign-keys-to-landmark ?1 [(control kp-end)] [(control shift kp-end)] [(kp-end)])
  (circul-assign-keys-to-landmark ?2 [(control kp-2)] [(control shift kp-2)] [(kp-2)])
  (circul-assign-keys-to-landmark ?2 [(control kp-down)] [(control shift kp-down)] [(kp-down)])
  (circul-assign-keys-to-landmark ?3 [(control kp-3)] [(control shift kp-3)] [(kp-3)])
  (circul-assign-keys-to-landmark ?3 [(control kp-next)] [(control shift kp-next)] [(kp-next)])
  (circul-assign-keys-to-landmark ?4 [(control kp-4)] [(control shift kp-4)] [(kp-4)])
  (circul-assign-keys-to-landmark ?4 [(control kp-left)] [(control shift kp-left)] [(kp-left)])
  (circul-assign-keys-to-landmark ?5 [(control kp-begin)] [(control shift kp-5)] [(kp-begin)])
  (circul-assign-keys-to-landmark ?5 [(control kp-5)] [(control shift kp-5)] [(kp-5)])
  (circul-assign-keys-to-landmark ?6 [(control kp-right)] [(control shift kp-right)] [(kp-right)])
  (circul-assign-keys-to-landmark ?7 [(control kp-7)] [(control shift kp-7)] [(kp-7)])
  (circul-assign-keys-to-landmark ?7 [(control kp-home)] [(control shift kp-home)] [(kp-home)])
  (circul-assign-keys-to-landmark ?8 [(control kp-8)] [(control shift kp-8)] [(kp-8)])
  (circul-assign-keys-to-landmark ?8 [(control kp-up)] [(control shift kp-up)] [(kp-up)])
  (circul-assign-keys-to-landmark ?9 [(control kp-9)] [(control shift kp-9)] [(kp-9)])
  (circul-assign-keys-to-landmark ?9 [(control kp-prior)] [(control shift kp-prior)] [(kp-prior)]))



(provide 'circul-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; circul.el ends here
