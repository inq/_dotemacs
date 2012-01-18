(require 'auto-complete)
(require 'rcodetools)

(defvar ac-source-rcodetools
  '((init . (lambda ()
              (condition-case x
                  (save-excursion
                    (rct-exec-and-eval rct-complete-command-name "--completion-emacs-icicles"))
                (error) (setq rct-method-completion-table nil))))
    (candidates . (lambda ()
                    (all-completions
                     ac-target
                     (mapcar
                      (lambda (completion)
                        (replace-regexp-in-string "\t.*$" "" (car completion)))
                      rct-method-completion-table))))))

(defun ac-ruby ()
  (interactive)
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-rcodetools)))
  (message "ac-ruby loaded") )

(provide 'auto-complete-ruby)