;;; landmark.el --- Circulation over Buffers  -*- lexical-binding: t; -*-
;; Circulation over Buffers 
;;
;; Copyright (C) 2022 Pierre Courtieu
;;
;; Authors: Pierre Courtieu
;; Maintainer: Pierre Courtieu <Pierre.Courtieu@gmail.com>
;; URL: https://github.com/Matafou/landmark
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
;; This file provides two utilities for navigating quickly through
;; buffers and positions.
;;
;; 1) Easy cycling between "interesting" buffers. Interestingness
;;    being governed by user defined regexps.
;;
;; 2) So called "landmarks" feature. A landmark is a location where
;;   you might want to come back later. It is either a buffer (jumping
;;   back to a "buffer landmark" goes to current point position in the
;;   buffer) or a precise position in a buffer ("position landmark").
;;
;;   Like registers, landmarks are identified uniquely by a character.
;;
;;   One typically associates a key of the keyboard, say f1, to a
;;   landmark l so that hitting f1 itself jumps to l (which makes it
;;   much faster than any keybindings for registers), hitting C-f1
;;   sets l to current buffer and C-S-f1 sets l to current position.
;;   One can chose any key, numpad keys proved to be a good choice.
;;
;;   Positional landmarks are (by default) visible in the buffer.
;;
;; This file does not set any keybinding. Examples of key setting are
;; given in the comments below. Pre-defined keybindings can also be
;; set at once.
;;
;; Implementation is very light: buffer landmark are dealt with the
;; bury-buffer stock functions, and position landmark are dealt with
;; registers + overlays for visual feedback.
;;
;; TODO: Provide typical american keyboard configuration as an option?
;;
;;;;;;;;;;;;;;;;;; EXAMPLE OF SET-KEY ;;;;;;;;;;;;;;;;;;;;;
;; for cycling:
;; (global-set-key [(control kp-enter)]  'landmark-bury-buffer)
;; (global-set-key [(control kp-add)]  'landmark-unbury-buffer)
;; (global-set-key [(control meta mouse-4)]  'landmark-bury-buffer)
;; (global-set-key [(control meta mouse-5)]  'landmark-unbury-buffer)
;; (landmark-assign-keys ?9 [(control kp-prior)] [(control shift kp-prior)] [(kp-prior)])a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar landmark-doc nil
  "Documentation of the landmark feature from landmark.el.

Like emacs registers a landmark is a location where you might
want to come back later. Unlike registers it is

- either a buffer (jumping back to a \"buffer landmark\" jumps to
  the buffer at its current point position)

- or a precise position in a buffer (\"position landmark\").

Like registers, each landmark is identified uniquely by a
character but this is anecdotical. Unlike registers it is also
attached to a key of your keyboard.

Say we take the f1 key for landmark ?1, then the following:

`(landmark-assign-three-standard-keys ?1 \'f1)'

makes so that:

- hitting C-f1 sets landmark ?1 to current buffer `landmark-of-buffer'

- hitting C-S-f1 sets landmark ?1 to current position `landmark-of-position'.

- hitting f1 itself jumps to the landmark ?1 (which makes it much
  faster than any keybindings for registers) (`landmark-jump').

One can chose any key but chosing a self inserting key would be
harmful since the self insertion would be lost. Numpad keys
is a good choice:

  (landmark-assign-three-standard-keys ?0 'kp-0)
  (landmark-assign-three-standard-keys ?0 'kp-insert)

See `landmark-assign-kp-n-config' to assign all numpad keys at once.

One can also change the modifiers (C- and C-S- above) at will
using the function `landmark-assign-keys'. Typically:

  (landmark-assign-keys ?1 [(meta kp-1)] [(control kp-1)] [(shift kp-1)])
  (landmark-assign-keys ?1 [(meta kp-end)] [(control kp-end)] [(shift kp-end)])

(Be careful with shift and numpad, as shift changes kp-1 into kp-end).

Positional landmarks are (by default) visible in the buffer. This
is controlled by `landmark-face', `landmark-show-landmark-position' and
`landmark-show-landmark-fringe'.
")

;;;###autoload
(defcustom landmark-uninteresting-buffer-regexp
  "\\`\\*\\|\\` \\|.+\\.log\\'"
  "Regexp of unintersting buffer names."
  :type '(string)
  :group 'landmark)

;;;###autoload
(defcustom landmark-uninteresting-buffer-exception-regexp 
  ""
  "Regexp of exceptions for `landmark-uninteresting-buffer-regexp'."
  :type '(string)
  :group 'landmark)

;;;###autoload
(defcustom landmark-show-landmark-fringe t  
  "If non-nil show landmarks in the fringe.

Both `landmark-show-landmark-fringe' and
`landmark-show-landmark-position' can be non-nil."
  :type '(boolean)
  :group 'landmark)

(defcustom landmark-show-landmark-position t  
  "If non-nil show landmarks with a special face.

Both `landmark-show-landmark-fringe' and
`landmark-show-landmark-position' can be non-nil. Changing this
variable only affects the future landmarks."
  :type '(boolean)
  :group 'landmark)

(defvar landmark-buffer-alist nil "List of buffers currently assigned to landmark.")
(defvar landmark-overlay-alist nil "List of overlays of landmarks.")


;;;###autoload
(defun landmark-is-uninteresting-buffer (buf)
  "Return t if string BUF is not an interesting buffer name.
See `landmark-uninteresting-buffer-regexp'
and `landmark-uninteresting-buffer-exception-regexp'."
  (and (string-match landmark-uninteresting-buffer-regexp buf)
       (not (string-match landmark-uninteresting-buffer-exception-regexp buf))))

;;;###autoload
(defun landmark-is-interesting-buffer (buf)
  "Return t if string BUF is an interesting buffer name.
See `landmark-uninteresting-buffer-regexp'
and `landmark-uninteresting-buffer-exception-regexp'."
  (not (landmark-is-uninteresting-buffer buf)))

(defun landmark--find-first-interesting (list-buf)
  "Return the first interesting buffer in LIST-BUF, nil if none exist."
  (buffer-name
   (car
    (cl-member-if (lambda (l) (landmark-is-interesting-buffer (buffer-name l))) list-buf))))

(defun landmark--find-last-interesting (list-buf)
  "Return the most buried interesting buffer in LIST-BUF, nil if none exist."
  (landmark--find-first-interesting (reverse list-buf)))

;;;###autoload
(defun landmark-bury-buffer ()
  "Bury current buffer and switch to:
- first interesting buffer in (buffer-list) if it exists
     - first buffer of (buffer-list) otherwise"
  (interactive)
  (bury-buffer)
  (let* ((intbuf (landmark--find-first-interesting (buffer-list))))
    (if (eq intbuf nil) () (switch-to-buffer intbuf))))

;;;###autoload
(defun landmark-unbury-buffer ()
  "Unbury the most buried interesting buffer if it exists.

Unbury the most buried non interesting buffer otherwise."
  (interactive)
  (let* ((intbuf (landmark--find-last-interesting (buffer-list))))
    (if (eq intbuf nil) (unbury-buffer) (switch-to-buffer intbuf))))

(defun landmark--add-assoc (k el l)
  "Add (K . EL) in the list L."
  (cons (cons k el) l))

;;; Assigning landmarks to buffers

;;;###autoload
(defun landmark--unbury-buffer (n)
  "Unbury buffer landmark N."
  (let* ((buf (cdr (assoc n landmark-buffer-alist))))
    (if (eq buf nil) (error "No buffer assign to this")
      (switch-to-buffer buf))))

;; Remove a killed buffer from the list
(add-hook
 'kill-buffer-hook 
 (lambda ()
   (let ((bufnme (buffer-name (current-buffer))))
     (setq landmark-buffer-alist (rassq-delete-all bufnme landmark-buffer-alist)))))

;;;###autoload
(defun landmark-of-buffer (n)
  "Assign landmark N to the current buffer.
Delete previous landmark N in the process."
  (interactive)
  (setq register-alist (assq-delete-all n register-alist))
  (setq landmark-buffer-alist 
	(landmark--add-assoc n (buffer-name (car (buffer-list))) 
		           landmark-buffer-alist)))

;;;###autoload
(defface landmark-face
  '((t (:box (:line-width (-1 . -1) :color "yellow")) ))
  "Used to mark register positions in a buffer."
  :group 'faces)

(defun landmark--grow-overlay (ov beg end)
  "Grow an overlay by one char if possible, preferably backward.

BEG and END are the beginning and end of the overlay.
Argument OV is the overlay."
  (if (not (eq (point-min) beg)) (move-overlay ov (- beg 1) end)
    (if (not (eq (point-max) end)) (move-overlay ov beg (+ 1 end)))))

;;;###autoload
(defun landmark-of-position (n)
  "Assign landmark N to the current position.
Delete previous landmark N in the process."
  (interactive)
  (setq landmark-buffer-alist (assq-delete-all n landmark-buffer-alist))
  (let ((oldov (cdr (assq n landmark-overlay-alist)))
        (m (point-marker)))
    (and oldov (delete-overlay oldov))
    ;; this make the register and overlay move accordingly
    ;; point-to-register does not seem to allow this
    (set-marker-insertion-type m t)
    (set-register n m)
    (let ((sovl (make-overlay (point)(+ 1 (point)) nil t nil)))
      (and landmark-show-landmark-position
           (overlay-put sovl 'face 'landmark-face))
      (overlay-put sovl 'help-echo (concat "Register: `" (char-to-string ?n) "'"))
      (and landmark-show-landmark-fringe
           (overlay-put sovl 'before-string (propertize "A" 'display '(left-fringe right-triangle))))
      ;; The overlay  should remain only one char long i possible.
      (overlay-put
       sovl 'modification-hooks
       (list (lambda (ov x beg end &optional lgthbefore)
               (if (and x (> lgthbefore 0))
                   (landmark--grow-overlay ov beg end)))))
      (setq landmark-overlay-alist (landmark--add-assoc n sovl landmark-overlay-alist)))))

;;;###autoload
(defun landmark-jump (n)
  "Jump to the register or the buffer named N."
  (interactive)
  (if (cdr (assoc n landmark-buffer-alist)) (landmark--unbury-buffer n)
    (jump-to-register n)))

;;;###autoload
(defun landmark-assign-keys (r kpos kbuf kgoto)
  "Sets keybindings related to landmark R.

Main landmark key binding function. A landmark is either a buffer
or a position in a buffer associated to a character (like
registers). This function assigns keybindings to three usual*
commands acting on the landmark named R:

KPOS: assign current position to R (`landmark-of-position').
KBUF: assign current buffer to R (`landmark-of-buffer').
KGOTO: jump to R (`landmark-jump').
ex:
 (landmark-assign-keys ?0 [(control f5)] [(control shift f5)] [(f5)])
 (landmark-assign-keys ?1 [(control kp-insert)] [(control shift kp-0)] [(kp-insert)])

See `landmark-doc'."
  (global-set-key kpos  `(lambda () (interactive) (landmark-of-buffer ,r)))
  (global-set-key kbuf  `(lambda () (interactive) (landmark-of-position ,r)))
  (global-set-key kgoto `(lambda () (interactive) (landmark-jump ,r))))

;;;###autoload
(defun landmark-assign-three-standard-keys (r key)
  "Assign KEY C-KEY and C-S-KEY to landmark R, as in `landmark-assign-keys'.

Quick configuration wrapper. This is equivalent to:

(landmark-assign-keys r [(control key)] [(control shift key)] [(key)])

R is acharacter and key is a symbol corresponding to a key. See
`landmark-assign-keys' for more details.

example use:

(landmark-assign-three-standard-keys ?1 'f1)

See `landmark-doc'.
"
  (let* ((lkey `(,key))
        (controlr `(control ,key))
        (controlshiftr `(control shift ,key)))
    ;;(message "(landmark-assign-keys %S %S %S %S)" r (vector controlr) (vector controlmetar) (vector lkey))
    (landmark-assign-keys r (vector controlr) (vector controlshiftr) (vector lkey))))

;;;###autoload
(defun landmark-assign-kp-n-config ()
  "Bind numpad keys kp-0 to kp-9 to landmark commands.

Sets kp-0 to kp-9 (and there equivalent if verr num key is on) to
usual landmark functions. See `landmark-doc'.

Warning: this tries to intercept all versions of numpad number
keys, whatever the state of the verr num switch.

See `landmark-doc'.
"
  (interactive)
  (landmark-assign-three-standard-keys ?0 'kp-0)
  (landmark-assign-three-standard-keys ?0 'kp-insert)
  (landmark-assign-three-standard-keys ?1 'kp-1)
  (landmark-assign-three-standard-keys ?1 'kp-end)
  (landmark-assign-three-standard-keys ?2 'kp-2)
  (landmark-assign-three-standard-keys ?2 'kp-down)
  (landmark-assign-three-standard-keys ?3 'kp-3)
  (landmark-assign-three-standard-keys ?3 'kp-next)
  (landmark-assign-three-standard-keys ?4 'kp-4)
  (landmark-assign-three-standard-keys ?4 'kp-left)
  (landmark-assign-three-standard-keys ?5 'kp-begin)
  (landmark-assign-three-standard-keys ?5 'kp-5)
  (landmark-assign-three-standard-keys ?6 'kp-right)
  (landmark-assign-three-standard-keys ?7 'kp-7)
  (landmark-assign-three-standard-keys ?7 'kp-home)
  (landmark-assign-three-standard-keys ?8 'kp-8)
  (landmark-assign-three-standard-keys ?8 'kp-up)
  (landmark-assign-three-standard-keys ?9 'kp-9)
  (landmark-assign-three-standard-keys ?9 'kp-prior))

(defun landmark-assign-fn-config ()
  "Bind f5-f9 to landmark commands.

Sets f5 to f9 to usual landmark functions. See `landmark-doc'.

Other f keys are bound by default in emacs. If you want to
override them, put this in your init file:

(landmark-assign-three-standard-keys ?1 'f1)
(landmark-assign-three-standard-keys ?2 'f2)

See `landmark-doc'."
  (interactive)
  (landmark-assign-three-standard-keys ?5 'f5)
  (landmark-assign-three-standard-keys ?6 'f6)
  (landmark-assign-three-standard-keys ?7 'f7)
  (landmark-assign-three-standard-keys ?8 'f8)
  (landmark-assign-three-standard-keys ?9 'f9))

;;;###autoload
(defun landmark-assign-mwheel-config ()
  "Bind (control meta mwheel) to cycle interesting buffers.

See `landmark-doc'."
  (interactive)
  (global-set-key [(control meta mouse-4)]  'landmark-bury-buffer)
  (global-set-key [(control meta mouse-5)]  'landmark-unbury-buffer))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'landmark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; landmark.el ends here
