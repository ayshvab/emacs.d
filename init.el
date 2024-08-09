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
  (ef-themes-select 'ef-spring))

(require 'recentf)
(recentf-mode 1)

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(setq make-backup-files nil)

(with-eval-after-load 'dired
  (require 'dired-x))

(pixel-scroll-precision-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)

(global-set-key (kbd "C-h") 'delete-backward-char)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(global-set-key [f2] 'recompile)

(use-package eat)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages '(eat ef-themes terraform-mode)))

