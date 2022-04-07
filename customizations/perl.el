
;perl
;
(defalias 'perl-mode 'cperl-mode)
(defun my-cperl-mode-hook ()
  (setq indent-tabs-mode t
		cperl-indent-level 4
		cperl-close-paren-offset -4
		cperl-continued-statement-offset 4
		cperl-indent-parens-as-block t
		cperl-tab-always-indent t))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

(defun my-cperl-indent-command ()
  "indent as cperl normally
  indent relatively inside multi-line strings.
  "
  (interactive)
  (let ((state (syntax-ppss)))
    (if (and (nth 3 state)              ;string
             (and (nth 8 state)         ;multi-line?
                  (< (nth 8 state) (point-at-bol))))
      (indent-relative)
      (cperl-indent-command))))
(eval-after-load "cperl-mode" '(define-key cperl-mode-map [remap cperl-indent-command] 'my-cperl-indent-command))
