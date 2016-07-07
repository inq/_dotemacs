;;; Code: init.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'haskell-mode-hook #'company-mode)
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/dev/rustc-1.8.0/src/")

(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
          (indent-according-to-mode)))

(global-set-key (kbd "TAB") #'complete-or-indent) ;
(setq company-tooltip-align-annotations t)

(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(mapcar #'(lambda (arg) (apply #'set-face-attribute arg))
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


(setq indent-tabs-mode nil)
(setq python-indent-offset 2)
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq python-indent 2)))

(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)


; TRAILING LINES
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file, even the last one"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines)
      (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
        (if (> trailnewlines 0)
            (progn
              (delete-char trailnewlines)))))))
(add-hook 'before-save-hook 'delete-trailing-blank-lines)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
(add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

;(providd 'init)
;;; init.el ends here