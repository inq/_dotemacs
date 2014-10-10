(windmove-default-keybindings 'meta)
(define-key global-map [select] 'windmove-up)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

; turn off toolbar
(if window-system
    (tool-bar-mode -1))
(desktop-save-mode 1)

; HASKELL
(add-to-list 'load-path "~/.emacs.d/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/haskell-mode/")

; S
(add-to-list 'load-path "~/.emacs.d/s")

; AUTO-COMPLETE
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\t" 'ac-expand)

; DASH
(add-to-list 'load-path "~/.emacs.d/dash")

; SCALA

(add-to-list 'load-path "~/.emacs.d/scala-mode2")
(require 'scala-mode2)
(add-to-list 'load-path "~/.emacs.d/sbt-mode")
(require 'sbt-mode)
(add-to-list 'load-path "~/.emacs.d/ensime-emacs")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

; MODE-COMPILE
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)


; KOREAN
(setq default-input-method "korean-hangul2")
(setq default-korean-keyboard "2")

(add-to-list 'load-path "/home/q/.emacs.d/")


; ANYTHING
;(add-to-list 'load-path "~/.emacs.d/anything")
;(require 'anything-config)


; RUBY
(add-to-list 'load-path "~/.emacs.d/ruby/")
(setq popup-use-optimized-column-computation nil)

(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")


(defun ruby-rcodetools-and-ri-and-auto-complete ()
  (require 'anything-rcodetools)
  (require 'auto-complete-ruby)
  (ac-ruby))

(add-hook 'ruby-mode-hook 'ruby-rcodetools-and-ri-and-auto-complete)


(require 'highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#333")

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

(setq inhibit-splash-screen t)


(require 'linum)
(setq linum-format "%4d ")
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

; YASNIPPET
(add-to-list 'load-path "~/.emacs.d/yasnippet/")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")

; XSCHEME
(require 'xscheme)

; DISABLE AUTOSAVING
(setq make-backup-files nil)
(setq auto-save-default nil)

; DELETE
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun backward-delete-word (arg)1
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-set-key (read-kbd-macro "<f5>") 'compile)

; ESHELL
(setq eshell-save-history-on-exit t)
(require 'ansi-color)
(require 'eshell)
(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))
(setq eshell-output-filter-functions 'eshell-handle-ansi-color)
(defun eshell/e (args)
  (find-file args))

; RINARI
(require 'ido)
(ido-mode t)

; CSCOPE
(add-to-list 'load-path "~/.emacs.d/xcscope")
(require 'xcscope)

; Start
(setq inhibit-splash-screen t)
(when (= (length command-line-args) 1)
  (eshell))

(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
