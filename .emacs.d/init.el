(setq-default indent-tabs-mode nil)
(setq tab-width 2) 

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; YASNIPPET
;; SEMANTIC
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;; RTAGS
(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

;; COMPANY
(require 'company)
(global-company-mode)

(push 'company-rtags company-backends)

;; HELM
(require 'helm-config)
(require 'rtags-helm)
(setq rtags-use-helm t)
(setq rtags-display-result-backend 'helm)

;; NLINUM
(global-nlinum-mode)

;; C++
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; RUST
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
;(setq racer-rust-src-path "~/Code/rust/src") ;; Rust source code PATH

;; COLUMN INDICATOR
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(setq fci-rule-character-color "darkgrey")
(setq fci-rule-column 80)
(setq fci-rule-character ?┃)
(global-fci-mode 1)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (web-mode typescript-mode flycheck-rust cargo fill-column-indicator google-c-style protobuf-mode csv-mode haskell-mode racer rust-mode nlinum helm rtags))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
