(setq circul-uninteresting-buffer-regexp
      "\\`\\*\\|\\` \\|.+\\.log\\'\\|bbdb\\|TAGS")
(setq circul-uninteresting-buffer-exception-regexp 
      "\\`\\*coq\\|\\`\\*cvs\\*")

; circuler en ignorant les buffers inintéressants (commençants par *,
; configurable):
(global-set-key [(control kp-enter)]  'circul-bury-buffer)
(global-set-key [(control kp-add)]  'circul-unbury-buffer)
(global-set-key [(control meta button4)]  'circul-bury-buffer)
(global-set-key [(control meta button5)]  'circul-unbury-buffer)


; assignation de caractère au buffer, et rappel d'un buffer numerote
; de 0 a 9 assignation: 

;  C-kp0...Ckp-9 assigne un buffer
;  C-S-kp0...Ckp-S-9 assigne une position dans un buffer
;  kp0...kp9 rappelle le buffer ou la position
;  en principe il ne peut pas y avoir un caractère associé aux deux à
;  la fois si on n'utilise que les deux fonctions circul-set

(circul-assign ?0 [(control kp-0)] [(control shift kp-0)] [(kp-0)])
(circul-assign ?0 [(control kp-insert)] [(control shift kp-insert)] [(kp-insert)])
(circul-assign ?1 [(control kp-1)] [(control shift kp-1)] [(kp-1)])
(circul-assign ?1 [(control kp-end)] [(control shift kp-end)] [(kp-end)])
(circul-assign ?2 [(control kp-2)] [(control shift kp-2)] [(kp-2)])
(circul-assign ?2 [(control kp-down)] [(control shift kp-down)] [(kp-down)])
(circul-assign ?3 [(control kp-3)] [(control shift kp-3)] [(kp-3)])
(circul-assign ?3 [(control kp-next)] [(control shift kp-next)] [(kp-next)])
(circul-assign ?4 [(control kp-4)] [(control shift kp-4)] [(kp-4)])
(circul-assign ?4 [(control kp-left)] [(control shift kp-left)] [(kp-left)])
(circul-assign ?5 [(control kp-begin)] [(control shift kp-5)] [(kp-begin)])
(circul-assign ?5 [(control kp-5)] [(control shift kp-5)] [(kp-5)])
(circul-assign ?6 [(control kp-right)] [(control shift kp-right)] [(kp-right)])
(circul-assign ?7 [(control kp-7)] [(control shift kp-7)] [(kp-7)])
(circul-assign ?7 [(control kp-home)] [(control shift kp-home)] [(kp-home)])
(circul-assign ?8 [(control kp-8)] [(control shift kp-8)] [(kp-8)])
(circul-assign ?8 [(control kp-up)] [(control shift kp-up)] [(kp-up)])
(circul-assign ?9 [(control kp-9)] [(control shift kp-9)] [(kp-9)])
(circul-assign ?9 [(control kp-prior)] [(control shift kp-prior)] [(kp-prior)])

(provide 'circul-keys-pc)
