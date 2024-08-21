(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("nongnu" . "https://elpa.nongnu.org/nongnu/")
	     )
(package-initialize)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "CaskaydiaMono Nerd Font")))))

(use-package ef-themes
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-duo-light))

(global-font-lock-mode 0)

(use-package emacs
  :config
  (defun dir-concat (dir file)
    "join path DIR with filename FILE correctly"
    (concat (file-name-as-directory dir) file))

  ;; Adds ~/.emacs.d to the loadp-path
  (push (dir-concat user-emacs-directory "plugins/") load-path)
  (push (dir-concat user-emacs-directory "lisp/") load-path)
  (defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by emacs are placed."))

;; (setq xah-fly-use-control-key nil)
;; (setq xah-fly-use-meta-key nil)
;; (require 'xah-fly-keys)
;; (xah-fly-keys-set-layout "qwerty")
;; (xah-fly-keys 1)
;; (setq xah-fly-command-mode-indicator "🔺")
;; (global-set-key (kbd "M-SPC") 'xah-fly-command-mode-activate)
;; (global-set-key (kbd "M-j") 'xah-fly-command-mode-activate)
;; (global-set-key (kbd "<f9>") xah-fly-leader-key-map)

;; ;; 2021-03-10 this fix the problem of: when emacs start as daemon, xah fly keys is not in command mode. thx to David Wilson (daviwil)
;; (defun my/server-fix-up()
;;   "Make sure 'xah-fly-keys' is starting in command-mode.

;; https://github.com/xahlee/xah-fly-keys/issues/103
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Tips-08.org#configuring-the-ui-for-new-frames"
;;   (xah-fly-keys-set-layout "qwerty")
;;   (xah-fly-keys t))

;; (if (daemonp)
;;     (add-hook 'server-after-make-frame-hook 'my/server-fix-up))




(require 'recentf)
(recentf-mode 1)




(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(setq make-backup-files nil)

(with-eval-after-load 'dired
  (require 'dired-x))

(pixel-scroll-precision-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.01)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;; ----------------------------
;; Editing
;; ----------------------------
(delete-selection-mode 1)
(global-set-key (kbd "C-h") 'delete-backward-char)
(setq scroll-conservatively 1)
(setq scroll-preserve-screen-position t)

(global-set-key
 (kbd "C-w")
 (defun backward-kill-word-or-region (&optional arg)
   "Kill word backwards unless region is active,
kill region instead"
   (interactive)
   (if (region-active-p)
       (kill-region (region-beginning)
				      (region-end))
     (backward-kill-word (or arg 1)))))

(advice-add 'kill-ring-save :around
	    (defun kill-ring-save-advice (fun &rest args)
	      "Save line to kill ring if region is inactive"
	      (interactive)
	      (if mark-active
		  (funcall fun (region-beginning) (region-end))
		(funcall fun (line-beginning-position)
			 (line-beginning-position 2)))))

(global-set-key (kbd "C-c SPC") 'whitespace-mode)

(defun back-to-indentation-or-beginning () (interactive)
		       (if (= (point) (progn (back-to-indentation) (point)))
			   (beginning-of-line)))

(define-key (current-global-map) [remap beginning-of-line] 'back-to-indentation-or-beginning)

(repeat-mode 1)

(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward
  )

(defvar-keymap ctrl-move-repeat-map
    :repeat t
    "f" #'forward-char
    "b" #'backward-char
    "n" #'next-line
    "p" #'previous-line
    "a" #'back-to-indentation-or-beginning
    "e" #'move-end-of-line
    "SPC" #'set-mark-command)

(defvar-keymap ctrl-delete-repeat-map
    :repeat t
    "d" #'delete-char
    "k" #'kill-line)

(defvar-keymap alt-move-repeat-map
    :repeat t
    "f" #'forward-word
    "b" #'backward-word
    "a" #'backward-sentence
    "e" #'forward-sentence
    "{" #'backward-paragraph
    "}" #'forward-paragraph
    "SPC" #'set-mark-command
    )

;; Occur:
(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)

(use-package deadgrep
  :ensure t
  :config
  (global-set-key (kbd "<f7>") #'deadgrep))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(global-set-key [f2] 'recompile)

(use-package eat)

(use-package popper
  :ensure t ; or :straight t
  :bind (("M-~" . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-c M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints


;;
;; Windows
;; 

(use-package windmove
  :bind
  (("C-c w <right>" . windmove-swap-states-right)
   ("C-c w <down>" . windmove-swap-states-down)
   ("C-c w <up>" . windmove-swap-states-up)
   ("C-c w <left>" . windmove-swap-states-left)))

;;----------------------------------------------------------------
;; ** switchy-window
;;----------------------------------------------------------------

(use-package switchy-window
  :ensure t
  :defer 2
  :init (switchy-window-minor-mode)
  :bind (("M-o" . switchy-window)
         :map other-window-repeat-map
         ("o" . switchy-window))
  :config
  (put 'switchy-window 'repeat-map 'other-window-repeat-map))

(use-package window
  :unless (fboundp 'switchy-window-minor-mode)
  :bind (("M-o" . my/other-window)
         ("M-O" . my/other-window-prev)
         :map other-window-repeat-map
         ("o" . my/other-window)
         ("O" . my/other-window-prev))
  :config
  (defalias 'my/other-window
    (let ((direction 1))
      (lambda (&optional arg)
        "Call `other-window', switching directions each time."
        (interactive)
        (if (equal last-command 'my/other-window)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1)))))))
  (defun my/other-window-prev (&optional arg all-frames)
    (interactive "p")
    (other-window (if arg (- arg) -1) all-frames))
  (put 'my/other-window 'repeat-map 'other-window-repeat-map)
  (put 'my/other-window-prev 'repeat-map 'other-window-repeat-map))


;;
;; Auto-revert
;;

(use-package autorevert
  :hook ((prog-mode
          text-mode
          tex-mode
          org-mode
          conf-mode) . auto-revert-mode))


(require 'tramp)




(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
	 ("\\.ts\\'"  . tsx-ts-mode)
	 ("\\.js\\'"  . js-ts-mode)
	 ("\\.yml\\'" . yaml-ts-mode)
	 ("\\.yaml\\'" . yaml-ts-mode)
	 ("\\*Dockerfile*\\'" . dockerfile-ts-mode)
	 )
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
              '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                (toml "https://github.com/tree-sitter/tree-sitter-toml")
                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
		(dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0" "src"))
		))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
         '((python-mode . python-ts-mode)
           (css-mode . css-ts-mode)
           (typescript-mode . typescript-ts-mode)
           (js2-mode . js-ts-mode)
           (bash-mode . bash-ts-mode)
           (css-mode . css-ts-mode)
           (json-mode . json-ts-mode)
           (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars))

(use-package terraform-mode
  ;; if using package.el
  :ensure t
  :custom (terraform-indent-level 4)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

;; (use-package nov
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(nov popper switchy-window almost-mono-themes deadgrep eat ef-themes terraform-mode)))

;;;
;;; TODO

;; Avy can do anything
;; https://karthinks.com/software/avy-can-do-anything/#avy-actions

;; Dired history in Emacs
;; https://karthinks.com/software/avy-can-do-anything/#avy-actions

;; Advanced usage of Emacs isearch
;; https://blog.chmouel.com/posts/emacs-isearch/
