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

;; NLINUM
(global-nlinum-mode)

;; C++
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; COLUMN INDICATOR
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(setq fci-rule-character-color "darkgrey")
(setq fci-rule-column 80)
(setq fci-rule-character ?┃)
(global-fci-mode 1)

