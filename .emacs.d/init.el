;;; init.el --- The entry point.
;; inkyu, 2012.

;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; RUST
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'haskell-mode-hook #'company-mode)
(custom-set-variables
 '(racer-cmd "~/.cargo/bin/racer")
 '(racer-rust-src-path  "~/dev/rustc-1.8.0/src/"))

(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
          (indent-according-to-mode)))

(global-set-key (kbd "TAB") #'complete-or-indent) ;

;; COMPANY
(custom-set-variables
 '(company-tooltip-align-annotations t)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1))

;; LINE NUMBERS
(global-nlinum-mode t)
(custom-set-variables '(nlinum-format "%3d "))
(custom-set-faces
 `(linum ((t (:inherit (shadow default)
                       :background "#303030"
                       :foreground "white")))))

(dolist #'(lambda (arg) (apply #'set-face-attribute arg))
        '((default nil :family "Monaco"
            :foreground "powder blue" :background "black")
          (isearch nil :foreground "brown4" :background "palevioletred2")
          (font-lock-comment-face nil :foreground "chocolate1")
          (font-lock-constant-face nil :foreground "Aquamarine")
          (font-lock-type-face nil :foreground "PaleGreen")
          (font-lock-variable-name-face nil :foreground "LightGoldenrod")
          (font-lock-function-name-face nil :foreground "LightSkyBlue")
          (font-lock-preprocessor-face nil :foreground unspecified)
          (font-lock-string-face nil :foreground "LightSalmon")
                    (font-lock-keyword-face nil :foreground "Cyan1")))

(setq make-backup-files nil)
(setq auto-save-default nil)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(global-flycheck-mode)


(setq-default indent-tabs-mode nil)
(setq-default python-indent-offset 2)
(add-hook 'python-mode-hook
          (lambda ()
            (fci-mode)
            (custom-set-variables
             '(tab-width 2)
             '(python-indent 2))))

(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)


(require 'ede)
(global-ede-mode)

; TRAILING LINES
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file, even the last one."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines)
      (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
        (if (> trailnewlines 1)
            (progn
              (delete-char trailnewlines)))))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-blank-lines)

(add-hook 'change-major-mode-hook
          (lambda ()
            (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
            (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (or (eq major-mode 'text-mode) (eq major-mode 'eshell-mode) (string-prefix-p "*" (buffer-name)))
              (remove-hook 'font-lock-mode-hook 'hc-highlight-tabs)
              (remove-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)
              (hc-dont-highlight-tabs)
              (hc-dont-highlight-trailing-whitespace)))
          'APPEND)

(require 'highlight-chars)
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)


(require 'magit-gerrit)
(setq-default magit-gerrit-remote "gerrit")

;; MAGIT
(global-set-key (kbd "C-c m") 'magit-status)
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; W3M
(setq browse-url-browser-function 'w3m-browse-url)

;; ELPY
(elpy-enable)
(global-set-key (kbd "M-]") 'elpy-goto-definition)
(global-set-key (kbd "M-[") 'pop-tag-mark)
(setq elpy-syntax-check-command 'pylint)

;; RTAGS
(require 'rtags)
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("/home/inkyu/dev/navi")))
 '(package-selected-packages
   (quote
    (nlinum yaml-mode w3m rtags racer python-mode pylint magit-gitflow magit-gerrit json-mode hlinum highlight-chars google-c-style flycheck-rust flycheck-haskell fish-mode fill-column-indicator elpy ebal company-ghc column-marker ac-haskell-process))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'init)
;;; init.el ends here
