;;; ac-math-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-math" "ac-math.el" (0 0 0 0))
;;; Generated autoloads from ac-math.el

(defvar ac-source-latex-commands '((candidates . math-symbol-list-latex-commands) (symbol . "c") (prefix . ac-math-prefix)))

(defvar ac-source-math-latex '((candidates . ac-math-candidates-latex) (symbol . "l") (prefix . ac-math-prefix) (action . ac-math-action-latex)))

(defvar ac-source-math-unicode '((candidates . ac-math-candidates-unicode) (symbol . "u") (prefix . ac-math-prefix) (action . ac-math-action-unicode)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-math" '("ac-math-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-math-autoloads.el ends here
