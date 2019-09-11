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
					;(server-start)
					;make sure the following packages are installed
					; 1. The auctex, magic-latex-buffer, aspell-en, pdf-tools,
					; after installing pdf-tools from MELPA run M-x install pdf-tools
					; this will compile and install the package. But I think every thing is inside the directory.
(setq inhibit-startup-screen t); this will prevent the start up menu
(blink-cursor-mode -1) ;this will stop the cursor from blinking
;; (require 'doom-modeline); require doom-modeline
;; (doom-modeline-mode 1); enable the doom-modeline
(global-diff-hl-mode); enable diff highlight for current changes
(set-default-font "Monaco 12") ;this will set the default font to monaco and size 12
;(load-theme 'nimbus t) ;enables the nimbus theme
(load-theme 'monokai t);'grandshell t) ;enables grandshell theme
(load "auctex.el" nil t t); it loads auctex 
(load "preview-latex.el" nil t t) ; it enables latex preview
(require 'latex-pretty-symbols) ;enables lates pretty symbols
;(require 'magic-latex-buffer)
(require 'company-auctex) ; this requires company latex for autofilling
(require 'flymake)
(require 'auto-package-update)
(auto-package-update-maybe) ; to enable auto update of melpa packages
(setq auto-package-update-interval 14) ; set the update interval to 14 days
(setq auto-package-update-prompt-before-update t) ; ask before going to update
(setq auto-package-update-delete-old-versions t) ; delete old versions after updating
(setq auto-package-update-hide-results t) ; hide the update results after the update
(pdf-tools-install); pdf-tools install
;(require 'auctex-latexmk)
;(add-hook 'TeX-mode-hook 'magic-latex-buffer)
(add-to-list 'auto-mode-alist '("\\.tex$" .LaTeX-mode)) ;open all .tex files in LaTeX-mode
(defun flymake-get-tex-args (file-name)
(list "pdflatex"
(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook ;this are the hooks I want to enable during LaTeX-mode

	  (lambda()
	    (company-auctex-init); start company latex
	    (turn-on-reftex) ;enable reftex
	    (flymake-mode); flymake mode
	    (rainbow-delimiters-mode)
	    (setq TeX-auto-save t) ;enable autosave on during LaTeX-mode
	    (setq TeX-parse-self t) ; enable autoparsing
	    (setq TeX-save-query nil) ; 
	    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		  TeX-source-correlate-start-server t) ;use pdf-tools for default document view
	    (setq TeX-source-correlate-method 'synctex) ; enable synctex
 
	    (setq TeX-source-correlate-mode t) ; enable text-source-correlate using synctex
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
	    (turn-on-auto-fill) ; autofill enable for line breaks
	    (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends))
	    )


	  )





;;
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))); disable linum-mode if enabled in pdf-view mode.

;(setq TeX-PDF-mode t)
;;

;(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (monokai-theme grandshell-theme rainbow-delimiters company-math markdown-mode multi-term auto-package-update nimbus-theme company-auctex use-package diff-hl yasnippet ac-math auto-complete magic-latex-buffer latex-pretty-symbols pdf-tools))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



