(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)


(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package modus-themes
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-operandi-tinted :no-confirm))

(use-package emacs
  :straight nil
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  
  :config
  (defun dir-concat (dir file)
    "join path DIR with filename FILE correctly"
    (concat (file-name-as-directory dir) file))

  ;; Adds ~/.emacs.d to the loadp-path
  (push (dir-concat user-emacs-directory "plugins/") load-path)
  (push (dir-concat user-emacs-directory "lisp/") load-path)
  (defvar user-cache-directory "~/.cache/emacs/"
    "Location where files created by emacs are placed.")

  (global-font-lock-mode 0)

  (progn
    (pixel-scroll-precision-mode 1)
    (menu-bar-mode 0)
    (scroll-bar-mode 0)
    (tool-bar-mode 0)
    (blink-cursor-mode 0)
    (show-paren-mode 1)
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq echo-keystrokes 0.01)
    (setq ring-bell-function 'ignore)

    (file-name-shadow-mode 1)
    (minibuffer-depth-indicate-mode 1)
    (minibuffer-electric-default-mode 1))

  (progn
    (require 'recentf)
    (recentf-mode 1)
    )

  (defalias 'list-buffers 'ibuffer) ; make ibuffer default
  (setq make-backup-files nil)

  (progn ;; dired
    (with-eval-after-load 'dired
      (require 'dired-x))
    )

  (progn ;; Editing
    (delete-selection-mode 1)
    (global-set-key (kbd "C-h") 'delete-backward-char)
    (global-set-key (kbd "C-z") 'zap-up-to-char)
    (setq scroll-conservatively 1)
    (setq scroll-preserve-screen-position t)

    (defun backward-kill-word-or-region (&optional arg)
      "Kill word backwards unless region is active, kill region instead"
      (interactive)
      (if (region-active-p)
	  (kill-region (region-beginning)
		       (region-end))
	(backward-kill-word (or arg 1))))
    (global-set-key (kbd "C-w") 'backward-kill-word-or-region)
    (advice-add 'kill-ring-save :around
		(defun kill-ring-save-advice (fun &rest args)
		  "Save line to kill ring if region is inactive"
		  (interactive)
		  (if mark-active
		      (funcall fun (region-beginning) (region-end))
		    (funcall fun (line-beginning-position)
			     (line-beginning-position 2)))))
    )

  (global-set-key (kbd "C-c SPC") 'whitespace-mode)

  (defun back-to-indentation-or-beginning () (interactive)
	 (if (= (point) (progn (back-to-indentation) (point)))
	     (beginning-of-line)))
  (global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

  (repeat-mode 1)
  (global-set-key [f9] 'repeat)

  (progn
    (defvar-keymap isearch-repeat-map
      :repeat t
      "s" #'isearch-repeat-forward
      "r" #'isearch-repeat-backward
      )
    
    ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
    (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
    (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
    (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
    (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
    (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
    (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)


    ;; Isearch in other windows
    (defun isearch-forward-other-window (prefix)
      "Function to isearch-forward in other-window."
      (interactive "P")
      (unless (one-window-p)
	(save-excursion
	  (let ((next (if prefix -1 1)))
            (other-window next)
            (isearch-forward)
            (other-window (- next))))))

    (defun isearch-backward-other-window (prefix)
      "Function to isearch-backward in other-window."
      (interactive "P")
      (unless (one-window-p)
	(save-excursion
	  (let ((next (if prefix 1 -1)))
            (other-window next)
            (isearch-backward)
            (other-window (- next))))))

    (define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
    (define-key global-map (kbd "C-M-r") 'isearch-backward-other-window)
    
    )
  )

(defun xah-search-current-word ()
  "Call `isearch' on current word or selection.
“word” here is A to Z, a to z, and hyphen [-] and lowline [_], independent of syntax table.

URL `http://xahlee.info/emacs/emacs/modernization_isearch.html'
Version: 2015-04-09"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq xp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq xp2 (point))))
    (setq mark-active nil)
    (when (< xp1 (point))
      (goto-char xp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties xp1 xp2))))

(global-set-key [f8] 'xah-search-current-word)
;; (global-set-key (kbd "M-s w") 'xah-search-current-word)

(defvar xah-brackets '("“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠")
 "A list of strings, each element is a string of 2 chars, the left bracket and a matching right bracket.
Used by `xah-select-text-in-quote' and others.")

(defconst xah-left-brackets
  (mapcar (lambda (x) (substring x 0 1)) xah-brackets)
  "List of left bracket chars. Each element is a string.")

(defconst xah-right-brackets
  (mapcar (lambda (x) (substring x 1 2)) xah-brackets)
  "List of right bracket chars. Each element is a string.")

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-11-22 2023-07-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((prog2 (backward-char) (looking-at (regexp-opt xah-right-brackets)) (forward-char))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(global-set-key (kbd "M-m") 'xah-goto-matching-bracket)
(global-set-key (kbd "M-9") 'xah-backward-left-bracket)
(global-set-key (kbd "M-0") 'xah-forward-right-bracket)

;; Occur:
(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)

(use-package deadgrep
  :ensure t
  :config
  (global-set-key (kbd "<f7>") #'deadgrep))

(global-set-key [f2] 'save-buffer)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(global-set-key [f6] 'recompile)
(global-set-key [M-f6] 'project-compile)

(use-package popper
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
  (popper-echo-mode +1))



(use-package switchy-window
  :custom (switchy-window-delay 1.5) ;; That's the default value.
  :bind    (:map switchy-window-minor-mode-map
                 ;; Bind to separate key...
                 ("M-o" . switchy-window)
                 ;; ...or as `other-key' substitute (C-x o).
                 ("<remap> <other-window>" . switchy-window)
		 :map other-window-repeat-map
		 ("o" . switchy-window))
  
  :init
  (switchy-window-minor-mode)

  :config
  (put 'switchy-window 'repeat-map 'other-window-repeat-map)
  
  (defun my-pulse-line-on-window-selection-change (frame)
    (when (eq frame (selected-frame))
      (pulse-momentary-highlight-one-line)))
  (add-hook 'window-selection-change-functions
            #'my-pulse-line-on-window-selection-change)
  )

(use-package autorevert
  :hook ((prog-mode
          text-mode
          tex-mode
          org-mode
          conf-mode) . auto-revert-mode))


(use-package terraform-mode
  :custom (terraform-indent-level 4)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind
  (("C-j" . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   )

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package helpful)

(use-package avy
  :config
  (global-set-key (kbd "M-j") 'avy-goto-char-timer)
  
  (setq avy-keys '(?q ?e ?r ?y ?u ?o ?p
                      ?a ?s ?d ?f ?g ?h ?j
                      ?k ?l ?' ?x ?c ?v ?b
                      ?n ?, ?/))


  (defun avy-show-dispatch-help ()  
    (let* ((len (length "avy-action-"))
           (fw (frame-width))
           (raw-strings (mapcar
			 (lambda (x)
			   (format "%2s: %-19s"
				   (propertize
				    (char-to-string (car x))
				    'face 'aw-key-face)
				   (substring (symbol-name (cdr x)) len)))
			 avy-dispatch-alist))
           (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
           (strings-len (length raw-strings))
           (per-row (floor fw max-len))
           display-strings)
      (cl-loop for string in raw-strings
               for N from 1 to strings-len do
               (push (concat string " ") display-strings)
               (when (= (mod N per-row) 0) (push "\n" display-strings)))
      (message "%s" (apply #'concat (nreverse display-strings)))))

  ;; Kill text
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
	(alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)


  ;; Copy text
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
	(copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
	(alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)


  ;; Yank text
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
	(alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  ;; Avy + Isearch
  (define-key isearch-mode-map (kbd "M-j") 'avy-isearch)
  )


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package vertico
  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  :config
  )

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))


(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("99d1e29934b9e712651d29735dd8dcd431a651dfbe039df158aa973461af003e" "9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2" default)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "CaskaydiaMono Nerd Font Light")))))
