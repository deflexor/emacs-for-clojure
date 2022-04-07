;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

; autosave
(defun save-all ()
    (interactive)
    (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)


; charsets
;(set-coding-system-priority 'utf-8 'cp1251 'emacs-mule)

; tramp
(setq tramp-default-method "ssh")
;;(setq tramp-auto-save-directory "C:/Users/Guest")
(when (eq window-system 'w32)
  (setq tramp-default-method "plink"))

; save history
(savehist-mode 1)

; :E
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

; It's also nice to be able to see when a file actually ends.
;
;
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
    (toggle-indicate-empty-lines))

; Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)


; Useful funcs
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))



; Duplicate line
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key (kbd "M-f") 'duplicate-line-or-region)


;; vi-like match parens
(defun match-parenthesis (arg)
  "Match the current character according to the syntax table.

   Based on the freely available match-paren.el by Kayvan Sylvan.
   I merged code from goto-matching-paren-or-insert and match-it.

   You can define new \"parentheses\" (matching pairs).
   Example: angle brackets. Add the following to your .emacs file:

    (modify-syntax-entry ?< \"(>\" )
    (modify-syntax-entry ?> \")<\" )

   Simon Hawkin <cema@cs.umd.edu> 03/14/1998"
  (interactive "p")
  (let
      ((syntax (char-syntax (following-char))))
    (cond
     ((= syntax ?\()
      (forward-sexp 1) (backward-char))
     ((= syntax ?\))
      (forward-char) (backward-sexp 1))
     (t (message "No match"))
     )
    ))
(global-set-key (kbd "<f6>") 'match-parenthesis)


; search goodies
(defun my-isearch-word-at-point-back ()
  (interactive)
  (call-interactively 'isearch-backward-regexp)
  (my-isearch-yank-symbol))

(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp)
  (my-isearch-yank-symbol))

(defun my-isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (let ((sym (symbol-at-point)))
    (if sym
      (progn
        (setq isearch-regexp t
              ;isearch-string (concat "\\<" (regexp-quote (symbol-name sym)) "\\>")
              isearch-string (regexp-quote (symbol-name sym))
              isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
              isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(global-set-key (kbd "<f7>") 'my-isearch-word-at-point-back)
(global-set-key (kbd "<f8>") 'my-isearch-word-at-point)


(defun my-imenu-goto-def ()
  (interactive)
  (let ((curword (thing-at-point 'symbol)))
    (imenu curword)))

(defun my-imenu-go-function-list ()
    (interactive) 
      (let ((unread-command-events  (listify-key-sequence "func\n") ))
          (call-interactively 'imenu)))

(global-set-key (kbd "<f9>") 'my-imenu-goto-def)

; Making C-x k end an emacsclient session
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x C-k") 'server-edit)
              (local-set-key (kbd "C-x k") 'server-edit))))


; useful funtion to clear repl buffer
(defun my-clear-repl ()
    (interactive)
      (let ((comint-buffer-maximum-size 0))
            (comint-truncate-buffer)))
