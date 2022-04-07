; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
    ; Yas
    (yas-activate-extra-mode 'html-mode)
    ; tide
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1))
    ; es6
    (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled)))
    ;
    (when (string-equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx"))
    (setq web-mode-engines-alist '(("razor" . "\\.gohtml\\'")))
    (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
    (setq web-mode-style-padding 0)
    (setq web-mode-script-padding 0)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-indentation nil)
    (setq web-mode-enable-current-element-highlight t)
    (when (string-equal "tmpl" (file-name-extension buffer-file-name))
      (setq indent-tabs-mode t)
      (setq web-mode-css-indent-offset 4)
      (setq web-mode-markup-indent-offset 4)
      (setq web-mode-code-indent-offset 4)))
(add-hook 'web-mode-hook  'my-web-mode-hook)


;; Typescript
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode-on)))
