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
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'latex-pretty-symbols)
(require 'magic-latex-buffer)
(add-hook 'TeX-mode-hook 'magic-latex-buffer)
(add-hook 'TeX-mode-hook 'flyspell-mode)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(pdf-tools-install)
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
  TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-mode t)

(setq reftex-plug-into-AUCTeX t)
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magic-latex-buffer latex-pretty-symbols pdf-tools))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
