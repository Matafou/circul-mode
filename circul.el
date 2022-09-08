;;;;circul.el
;; Circulation over Buffers 

;; This file mixes registers and an adhoc list of buffers to provide
;; several features:
;;  * easy circulation between "interesting" buffers. Interestingness
;;    being governed by user defined regexps.
;;  * easy assignment of keys to buffer or registers.
;; this file does not set any keybinding, (an exemple of key setting
;; is given in the comments below). It is therefore not a minor mode.

;; TODO: Provide typical american keyboard configuration?

;;;;;;;;;;;;;;;;;; EXAMPLE OF SET-KEY ;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key [(control kp-enter)]  'circul-bury-buffer)
;; (global-set-key [(control kp-add)]  'circul-unbury-buffer)
;; (global-set-key [(control meta button4)]  'circul-bury-buffer)
;; (global-set-key [(control meta button5)]  'circul-unbury-buffer)

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

;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom circul-uninteresting-buffer-regexp
  "\\`\\*\\|\\` \\|.+\\.log\\'"
  "Regexp of unintersting buffer names.")

(defcustom circul-uninteresting-buffer-exception-regexp 
  ""
  "Regexp of exceptions for circul-uninteresting-buffer-regexp.")

(defvar circul-buffer-alist nil "List of assigned buffers.")
(defvar circul-overlay-alist nil "List of overlays of circul markers.")

(defun circul-is-uninteresting-buffer (buf)
  "return t if string BUF is not an interesting buffer name.
See `circul-uninteresting-buffer-regexp'
and `circul-uninteresting-buffer-exception-regexp'."
  (let* ((first-char (substring buf 0 1)))
    (and (string-match circul-uninteresting-buffer-regexp buf)
	 (not (string-match circul-uninteresting-buffer-exception-regexp buf)))))

(defun circul-is-interesting-buffer (buf)
  "return t if string BUF is an interesting buffer name.
See `circul-uninteresting-buffer-regexp'
and `circul-uninteresting-buffer-exception-regexp'."
  (not (circul-is-uninteresting-buffer buf)))

(defun circul-find-first-interesting (list-buf)
  "Return the first interesting buffer in LIST-BUF, nil if none exist."
  (buffer-name
   (car
    (cl-member-if (lambda (l) (circul-is-interesting-buffer (buffer-name l))) list-buf))))

(defun circul-find-last-interesting (list-buf)
  "Return the most buried interesting buffer in LIST-BUF, nil if none exist."
  (circul-find-first-interesting (reverse list-buf)))


(defun circul-bury-buffer ()
  "Bury current buffer and switch to:
     - first interesting buffer in (buffer-list) if it exists
     - first buffer of (buffer-list) otherwise"
  (interactive)
  (bury-buffer)
  (let* ((intbuf (circul-find-first-interesting (buffer-list))))
    (if (eq intbuf nil) () (switch-to-buffer intbuf))))

(defun circul-unbury-buffer ()
  "Unbury the most buried interesting buffer if it exists, unbury
the most buried buffer otherwise."
  (interactive)
  (let* ((intbuf (circul-find-last-interesting (buffer-list))))
    (if (eq intbuf nil) (unbury-buffer) (switch-to-buffer intbuf))))

(defun circul-add-assoc (k el l)
  "add (K . EL) in the list l."
  (cons (cons k el) l))

;;; Assigning number to buffers

(defun circul-assign-current-buffer (n)
  "Assign number n to the current buffer in circul-buffer-alist.
precondition: no repetition in the list."
  (interactive)
  (setq circul-buffer-alist 
	(circul-add-assoc n (buffer-name (car (buffer-list))) 
		   circul-buffer-alist)))

(defun circul-unbury-buffer-n (n)
  "Unbury buffer char n found in circul-buffer-alist"
  (interactive)
  (let* ((buf (cdr (assoc n circul-buffer-alist))))
    (if (eq buf nil) (error "No buffer assign to this")
      (switch-to-buffer buf))))

;; Remove a killed buffer from from the list
(add-hook
 'kill-buffer-hook 
 (lambda ()
   (let ((bufnme (buffer-name (current-buffer))))
     (setq circul-buffer-alist (rassq-delete-all bufnme circul-buffer-alist)))))


(defun circul-set-current-buf (n)
  "Assign char N to the current buffer in `circul-buffer-alist',
and suppress the register called N if it exists"
  (interactive)
  (setq register-alist (assq-delete-all n register-alist))
  (circul-assign-current-buffer n))

(defface circul-register-marker-face
  '((t (:box (:line-width (-1 . -1) :color "yellow")) ))
  "Used to mark register positions in a buffer."
  :group 'faces)

(defun circul--grow-overlay (ov beg end)
  "Grow an overlay by one char if possible, preferably backward.

BEG and END are the beginning and end of the overlay."
  (if (not (eq (point-min) beg)) (move-overlay ov (- beg 1) end)
    (if (not (eq (point-max) end)) (move-overlay ov beg (+ 1 end)))))

(defun circul-set-current-reg (n)
  "Assign register N to the current position,
and suppress the char N of `circul-buffer-alist' if it exists."
  (interactive)
  (setq circul-buffer-alist (assq-delete-all n circul-buffer-alist))
  (let ((oldov (cdr (assq n circul-overlay-alist)))
        (m (point-marker)))
    (and oldov (delete-overlay oldov))
    ;; this make the register and overlay move accordingly
    ;; and stay 0-length too. point-to-register does not
    ;; seem to allow this
    (set-marker-insertion-type m t)
    (set-register n m)
    (let ((sovl (make-overlay (point)(+ 1 (point)) nil t nil)))
      (overlay-put sovl 'face 'circul-register-marker-face)
      (overlay-put sovl 'help-echo (concat "Register: `" (char-to-string ?n) "'"))
      (overlay-put sovl 'before-string (propertize "A" 'display '(left-fringe right-triangle)))
      (overlay-put
       sovl 'modification-hooks
       (list (lambda (ov x beg end &optional lgthbefore)
               (if (and x (> lgthbefore 0))
                   (circul--grow-overlay ov beg end)
                 ))))
      (setq circul-overlay-alist (circul-add-assoc n sovl circul-overlay-alist) )
      )
    )
  )

(defun circul-jump-to-register-or-buffer (n)
  "Jump to the register or the buffer named N."
  (interactive)
  (if (cdr (assoc n circul-buffer-alist)) (circul-unbury-buffer-n n)
    (jump-to-register n)))

(defun circul-select-tab-buffers (bsel currbuf)
  (not (circul-is-uninteresting-buffer (buffer-name bsel))))

;; An all-in-one setter, typical use:
;; (circul-assign-register-to-key ?0
;;    [(control kp-insert)] [(control shift kp-0)] [(kp-insert)])
(defun circul-assign (r kpos kbuf kgoto)
  "Sets three keybindings related to character R.
KPOS will assigns current position to R.
KBUF will assigns current buffer to R.
KGOTO will jump to buffer or position assigned to R.
ex:
 (circul-assign ?0 [(control f5)] [(control shift f5)] [(f5)])
 (circul-assign ?1 [(control kp-insert)] [(control shift kp-0)] [(kp-insert)])"
  (global-set-key kpos 
		  `(lambda () (interactive) (circul-set-current-buf ,r)))
  (global-set-key kbuf 
		  `(lambda () (interactive) (circul-set-current-reg ,r)))
  (global-set-key kgoto 
		  `(lambda () (interactive) (circul-jump-to-register-or-buffer ,r))))


(provide 'circul)
