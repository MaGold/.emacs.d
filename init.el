;no start-up screen
(setq inhibit-startup-message t)

;MY KEY BINDINGS

;define a kbd macro to copy a line
(defun my-copy-line () (interactive) (kill-ring-save (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-S-c") 'my-copy-line) 

;define a kbd macro to insert new line below and go to it
(defun my-insert-line () (interactive) (move-end-of-line 1) (newline))
(global-set-key (kbd "C-S-n") 'my-insert-line) 

;C-x C-r to revert-buffer
(global-set-key [(control ?x) (control ?r)] 'revert-buffer)

;C-shift-l for linum-mode
(global-set-key (kbd "C-S-l") 'linum-mode)

;C-shift-f for flycheck-mode
(global-set-key (kbd "C-S-f") 'flycheck-mode)
;END MY KEYBINDINGS

; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;-------------------------------------------------------------------------------------------------------
;; Requisites: Emacs >= 24
(require 'package)			
(package-initialize)

;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository flycheck workgroups2))

(mapc 'install-if-needed to-install)

(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
;(global-flycheck-mode 1)

(global-set-key [f7] 'find-file-in-repository)

; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

;; ;; Python mode settings
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

;; ;; Jedi settings
(require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
;;       '("python2" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))

;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (jedi:setup)
;; 	    (jedi:ac-setup)
;;             (local-set-key "\C-cd" 'jedi:show-doc)
;;             (local-set-key (kbd "M-SPC") 'jedi:complete)
;;             (local-set-key (kbd "M-.") 'jedi:goto-definition)))


(add-hook 'python-mode-hook 'auto-complete-mode)

(ido-mode t)

; turon off externals
;; Remove scrollbars, menu bars, and toolbars
; when is a special form of "if", with no else clause, it reads:
; (when <condition> <code-to-execute-1> <code-to-execute2> ...)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;god-mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
;end god-mode


;set default file search directory
(setq default-directory "/cygdrive/d/projects/")

; turn on line numberings
;(global-linum-mode 1)
(add-hook 'python-mode-hook #'linum-mode)

;; -------------------- extra nice things --------------------
;; use shift to move around windows
(windmove-default-keybindings 'shift)
(show-paren-mode t)
 ; Turn beep off
(setq visible-bell nil)

;; theme stuff will get written here when using customize-theme
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")


;workgroups stuff
(require 'workgroups2)
(setq wg-prefix-key (kbd "C-c w"))
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")
;(wg-load "~/")
(workgroups-mode 1)
