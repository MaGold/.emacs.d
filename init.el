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

; copy current line, comment it out, and paste it to next line
(defun copy-comment-paste ()
  (interactive)
  (save-excursion
    (let ((current-line (thing-at-point 'line t)))
      (goto-char (line-beginning-position))
      (insert current-line)
      (comment-region (line-beginning-position 0)
                      (line-end-position 0)))))

(global-set-key (kbd "C-S-m") 'copy-comment-paste) 
;; Same as above but this leaves cursor at beginning of new line
;; (defun copy-comment-paste ()
;;   (interactive)
;;   (save-excursion
;;     (let ((current-line (thing-at-point 'line t)))
;;       (comment-region (line-beginning-position) (line-end-position))
;;       (goto-char (line-end-position))
;;       (open-line 1)
;;       (forward-line)
;;       (insert current-line)
;;       ))
;;   (forward-line))

;END MY KEYBINDINGS

(setq holiday-bahai-holidays nil)
(setq holiday-islamic-holidays nil)

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; set the size of rendered latex images in org mode
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))

;; don't show asterix in *bold* in org mode, etc...
(setq org-hide-emphasis-markers t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Capture templates
(setq org-capture-templates
      '(("n"               ; key
         "Note"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/notes.org" "Notes")  ; target
         "* %? %(org-set-tags)  :note: \n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
	("w"               ; key
         "Weights"         ; name
         table-line        ; type
         (file "~/Dropbox/org/habits/weights.org" )  ; target
	 "|%U|%^{weight}|%^{comment}|"
         :prepend t        ; properties
         :kill-buffer t    ; properties
        )

	("q"               ; key
	 "Quick note"      ; name
	 entry             ; type
	 (file "~/Dropbox/org/quicknotes.org")  ; target
	 "* %?\nEntered on %U\n" ;template
	 :prepend t        ; properties
	 :empty-lines 1    ; properties
	 :created t        ; properties
	 )
	("t"               ; key
	 "Time Log"      ; name
	 entry             ; type
	 (file "~/Dropbox/org/timelog.org")  ; target
	 "* %?\nEntered on %U\n" ;template
	 :prepend t        ; properties
	 :empty-lines 1    ; properties
	 :created t        ; properties
	 )
	("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org") "* \n%?\n" :clock-in t)
	("k" "Journal-night" entry (file+datetree "~/Dropbox/org/journal-night.org") "* \n%?\n" :clock-in t)
        ))

;; indentation for lists in org mode
(setq org-list-indent-offset 2)


;; can't seem to export to pdf with a bibliography without this
 (setq org-latex-pdf-process
       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
         "bibtex %b"
         "makeindex %b"
         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"))


;; where to store back-up files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;commenting
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)





;; Organize todos
;;
;;-----------------------------------------------------------------------
;; (setq org-todo-keywords
;;   '(
;;     (sequence "TODO" "|" "DONE")
;;     (type "READ")
;; ))

(setq org-todo-keywords
       '((sequence "TODO" "READ" "STUDY" "PAPERS" "CODE" "ANKI" "|" "DONE")))


(setq org-todo-keyword-faces
  '(("TODO" . (:foreground "#ff39a3" :weight bold))
("READ" . (:foreground "#c4efcd" :weight bold))
;;("STUDY" . (:foreground "white" :background "#4d4d4d" :weight bold))
("STUDY" . (:foreground "white" :weight bold))
("PAPERS" . (:foreground "#b3c6e5" :weight bold))
("CODE" . (:foreground "#bc86e0" :weight bold))
("ANKI" . (:foreground "#e2ab58" :weight bold))
))

;; sort by todo state (for todo list) and then time (for daily logs)
(setq org-agenda-sorting-strategy 
      '((agenda todo-state-up time-up)))

;; (setq org-agenda-sorting-strategy 
;;       '((agenda todo-state-up)
;;         (todo priority-down tag-up)
;;         (tags priority-down tag-keep)
;;         (search category-keep)))


;; (setq org-agenda-sorting-strategy 
;;       '((agenda tag-up)
;;         (todo priority-down tag-up)
;;         (tags priority-down tag-keep)
;;         (search category-keep)))


;; ;; Global agenda: sort todos by category
;; (setq org-agenda-sorting-strategy
;;       '((agenda category-up)
;;         (todo priority-down category-up)
;;         (tags priority-down category-keep)
;;         (search category-keep)))
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
      '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository flycheck workgroups2 god-mode org-ref interleave elpy))


;; (add-to-list 'package-archives
;;              '("elpy" . "https://jorgenschaefer.github.io/packages/"))

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
;; (require 'python-mode)
;; (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;; (setq py-electric-colon-active t)
;; (add-hook 'python-mode-hook 'autopair-mode)
;; (add-hook 'python-mode-hook 'yas-minor-mode)

;; ;; Jedi settings
;;(require 'jedi)
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


;; (add-hook 'python-mode-hook 'auto-complete-mode)

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
;;(setq default-directory "/cygdrive/d/projects/")

; turn on line numberings
;(global-linum-mode 1)
(add-hook 'python-mode-hook #'linum-mode)

(elpy-enable)

(setq elpy-rpc-python-command "python3")

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

(require 'org-ref-latex)
(require 'org-ref-url-utils)

(require 'org-ref-pdf)

;; increase size of rendered latex images
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2))

;; darker zenburn theme
(with-eval-after-load "zenburn-theme"
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; original `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
     `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg-2)))))))

(setq org-agenda-files (list "~/Dropbox/org/references/articles.org"
			     "~/Dropbox/org/quicknotes.org"
			     ;;"~/Dropbox/org/journal.org"
			     "~/Dropbox/org/timelog.org"
			     "~/Dropbox/org/birthdays.org"
			     ))
(setq org-ref-notes-directory "~/Dropbox/org/references/notes"
      org-ref-bibliography-notes "~/Dropbox/org/references/articles.org"
      org-ref-default-bibliography '("~/Dropbox/org/references/articles.bib")
      org-ref-pdf-directory "~/Dropbox/org/references/pdfs/")



(setq helm-bibtex-bibliography "~/Dropbox/org/references/articles.bib"
      helm-bibtex-library-path "~/Dropbox/org/references/pdfs"
      helm-bibtex-notes-path "~/Dropbox/org/references/articles.org")




;workgroups stuff
(require 'workgroups2)
(setq wg-prefix-key (kbd "C-c w"))
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")
;(wg-load "~/")
(workgroups-mode 1)
