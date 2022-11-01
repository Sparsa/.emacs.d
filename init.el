;;; Package -- summary
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)
;;; Commentary:
	;; Start server only when there is not one already
(require 'server)
(unless (server-running-p)
  (server-start))
;Emacs application framework configurations
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-file-manager)
(require 'eaf-mindmap)
(require 'eaf-git)
(require 'eaf-vue-demo)
(require 'eaf-pdf-viewer)
(require 'eaf-org-previewer)
(require 'eaf-browser)
(require 'eaf-demo)
(require 'eaf-terminal)
(require 'eaf-image-viewer)
(require 'eaf-markdown-previewer)
(require 'eaf-jupyter)
(require 'eaf-system-monitor)
;;End of emacs application framework config
;; adding custom list of applications I use for daily basis.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-checker-error-threshold 1000)
 '(package-selected-packages
   '( markdown-preview-mode julia-snail vterm jupyter julia-mode async good-scroll minions mood-line doom-modeline company-reftex quelpa htmlize ox-reveal cdlatex paradox graphviz-dot-mode rust-mode lsp-mode lsp-latex flycheck bison-mode magit monokai-theme grandshell-theme rainbow-delimiters company-math markdown-mode multi-term auto-package-update nimbus-theme company-auctex use-package diff-hl yasnippet ac-math auto-complete magic-latex-buffer latex-pretty-symbols))
 '(paradox-github-token t)
 '(pdf-cs-reverse-scrolling nil)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun print-elements-of-list (list)
       "Print each element of LIST on a line of its own."
       (while list
         (print (car list))
         (setq list (cdr list))))
(define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))
(dolist (p package-selected-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

(setq pixel-scroll-precision-mode t)
(global-set-key (kbd "C-x g") 'magit-status)
;====== Comapany Settings
(global-set-key (kbd "TAB") 'company-complete-common)
					;======= IDO Settings
(paradox-require 'ido)
(ido-mode t)
;===== Automatically load any changes in the disk
(global-auto-revert-mode t)
;===== Stop the startup screen					;
(setq inhibit-startup-screen t); this will prevent the start up menu
;===== Stop  cursor blinking
(blink-cursor-mode -1) ;this will stop the cursor from blinking
					;===== Doom-modeline
					;(require 'flycheck-grammarly)
;(setq flycheck-grammarly-check-time 0.8)
(mood-line-mode)
(setq mode-line-minor-modes t)
(minions-mode 1)


(global-diff-hl-mode); enable diff highlight for current changes
(set-frame-font "Monaco 12") ;this will set the default font to monaco and size 12
(load-theme 'monokai t);'grandshell t) ;enables grandshell theme
(load "auctex.el" nil t t); it loads auctex 
(load "preview-latex.el" nil t t) ; it enables latex preview

(paradox-require 'ox-reveal)
					;(require 'flymake)
;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

;===== Set package auto update settings

(paradox-require 'auto-package-update)
 ; to enable auto update of melpa packages
(setq auto-package-update-interval 14) ; set the update interval to 14 days
(setq auto-package-update-prompt-before-update t) ; ask before going to update
(setq auto-package-update-delete-old-versions t) ; delete old versions after updating
(setq auto-package-update-hide-results t) ; hide the update results after the update
(auto-package-update-maybe)


(add-hook 'after-init-hook 'global-company-mode)


(require 'latex)					;(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
(add-hook 'LaTeX-mode-hook ;this are the hooks I want to enable during LaTeX-mode
	  (lambda()
	    (turn-on-reftex) ;enable reftex
	   ; (turn-on-cdlatex) ; I am not using cdlatex mauch.
	    (set (make-local-variable 'company-backends) '((separate: company-reftex-labels company-reftex-citations) (separate: company-auctex-symbols company-auctex-environments company-capf company-auctex-macros) company-math-symbols-latex
	    company-latex-commands ))
	    (rainbow-delimiters-mode)
	    (setq TeX-auto-save t) ;enable autosave on during LaTeX-mode
	    (setq TeX-parse-self t) ; enable autoparsing
	    (setq TeX-save-query nil) ; 
	    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
	    (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
	    (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))
	    ;; 	  TeX-source-correlate-start-server t)
					;use pdf-tools for default document view
	    (setq predictive-latex-electric-environments 1)
	    ;; (add-to-list 'LaTeX-indent-environment-list "tikzpicture")
	    ;; (print-elemenets-of-list LaTeX-indent-environment-list)
	    ;; (add-to-list 'LaTeX-indent-environment-list "scope")
	    ;; (add-to-list 'LaTeX-indent-environment-list '("scope"))
	    ;; (add-to-list 'LaTeX-indent-environment-list '("figure"))
	    ;; (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))
	    ;; (print-elemenets-of-list LaTeX-indent-environment-list)
	    
	    (setq TeX-source-correlate-method 'synctex) ; enable synctex
 
	    (setq TeX-source-correlate-mode t) ; enable text-source-correlate using synctex
	    (TeX-fold-mode 1); enableing tex fold mode for better readability.
;;	    (TeX-fold-buffer 1)
	    (setq-default TeX-master nil) 
	    (global-set-key (kbd "C-c C-g") 'pdf-sync-forward-search) ;sync from text to pdf
	    (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer) ; reload pdf buffer
	    (setq reftex-plug-into-AUCTeX t) ; enable auctex
	    (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
	    (local-set-key [C-tab] 'TeX-complete-symbol) ;tex complete symbol
	    ; could be ispell as well, depending on your preferences
	    (setq ispell-program-name "aspell") 
; this can obviously be set to any language your spell-checking program supports
	    (setq ispell-dictionary "english") 
	    (flyspell-mode) ; flyspell mode enable
	    (flyspell-buffer); flyspell buffer
	    (turn-on-auto-fill)
	    (visual-line-mode)
	    (LaTeX-math-mode)
	    )
	  )

;(add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
;(add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)
(setq lsp-print-performance t)
;; ORG mode settings start from here



(add-hook 'org-mode-hook
	  (global-set-key (kbd "C-c l") 'org-store-link)
	  (global-set-key (kbd "C-c a") 'org-agenda) 
	  (global-set-key (kbd "C-c c") 'org-capture)
	  
	  )

;;;;; rust mode hook
(paradox-require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
;;;;;; julia mode hoook
(paradox-require 'julia-mode)
(paradox-require 'jupyter)
(use-package vterm
  :ensure t)

(use-package julia-snail
  :ensure t
  :requires vterm
  :hook (julia-mode . julia-snail-mode))
;; Paradox
(paradox-require 'paradox)
(paradox-enable)

