;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Highlight current line
(global-hl-line-mode 1)

; Keybinings
;
(global-set-key (kbd "RET") 'newline-and-indent)
;(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-p") 'hippie-expand)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s") 'delete-char)
(global-set-key (kbd "M-q") 'cua-paste)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-z") 'undo)
(global-set-key (kbd "C-SPC") 'mark-sexp)
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "M-r") 'replace-regexp)
(global-set-key (kbd "C-M-z") 'kill-emacs)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-`") 'bs-show)
(global-set-key [C-tab] 'other-window)
(global-set-key [C-next] 'bs-show)
(global-set-key [C-prior] 'bs-show)
(global-set-key [insert] 'bs-show)
;(global-set-key (kbd "M-[ h") 'beginning-of-line)                                 
;(global-set-key (kbd "M-[ f") 'end-of-line)

; Buffer cycling
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "M-`") 'switch-to-previous-buffer)

(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(global-set-key (kbd "M-e") 'copy-line)


;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))
; (save-place-mode 1)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
;(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq create-lockfiles nil)


;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

; Marking text
;
;
(cua-mode t)
(setq cua-enable-cua-keys nil)
(transient-mark-mode 1)
(delete-selection-mode t)

; stops killing/yanking interacting with primary X11 selection and start to use clipboard
(setq x-select-enable-primary nil)
(setq x-select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

; mouse in XTerm
(xterm-mouse-mode t)

; js2-mode
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx\\'" . coffee-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)


; Redo +
(require 'redo+)
(global-set-key (kbd "C-t") 'redo)

