(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;load customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; ido
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; smex: substitute space with hyphen 
(defadvice smex (around space-inserts-hyphen activate compile)
        (let ((ido-cannot-complete-command 
               `(lambda ()
                  (interactive)
                  (if (string= " " (this-command-keys))
                      (insert ?-)
                    (funcall ,ido-cannot-complete-command)))))
          ad-do-it))

;; ----------------
;; Org-mode custom bindings
;; ----------------
(add-hook 'org-mode-hook 
          (lambda ()
            (local-set-key "\M-n" 'outline-next-visible-heading)
            (local-set-key (kbd "ESC <down>") 'org-metadown)
            (local-set-key (kbd "ESC <up>") 'org-metaup)
            (org-indent-mode t)
            ))

;; load org agenda files recursively
(load-library "find-lisp")
(setq org-agenda-files (find-lisp-find-files "~/CloudStation/org" "\.org$"))

;; Auto save org files 
;;(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(setq org-todo-keywords
  '((sequence "TODO(t)" "STARTED(s)" "WAIT(w!)" "|" "DONE(d!)")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("STARTED" . "yellow")
        ("WAIT" . "lightyellow1")
        ("DONE" . (:foreground "green" :weight bold))))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
      

;;; A simple visible bell which works in all terminal types. Added manually because it is not available in MELPA stable
(require 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)

;; Ovveride better defaults
(menu-bar-mode 0)

;; Visual line mode
(visual-line-mode 1)

;; Full screen
(toggle-frame-fullscreen)
