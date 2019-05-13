(setq-default indent-tabs-mode nil)
(setq tab-width 2) 

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; PYTHON
(elpy-enable)

;; YASNIPPET
;; SEMANTIC
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;; NLINUM
(global-nlinum-mode)

;; C++
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; COLUMN INDICATOR
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(setq fci-rule-character-color "darkgrey")
(setq fci-rule-column 100)
(setq fci-rule-character ?┃)
(global-fci-mode 1)

;; RUST
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hool #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; HELM
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)

(require 'helm-fuzzier)
(helm-fuzzier-mode 1)

(setq-default helm-mode-fuzzy-match t)
(setq-default helm-M-x-fuzzy-match t)
(setq-default helm-completion-in-region-fuzzy-match t)


(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "C-x C-f") #'helm-projectile)

(helm-mode 1)

(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)

(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(with-eval-after-load 'flycheck
		        (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (racer flycheck-inline flycheck-rust rust-mode helm-projectile projectile git-gutter elpy helm-fuzzy-find helm-fuzzier helm nlinum fill-column-indicator))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
