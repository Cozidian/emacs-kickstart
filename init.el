;; Kickstart.emacs is *not* a distribution.
;; It's a template for your own configuration.

;; It is *recommeded* to configure it from the *config.org* file.
;; The goal is that you read every line, top-to-bottom, understand
;; what your configuration is doing, and modify it to suit your needs.

;; You can delete this when you're done. It's your config now. :)

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun start/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it. Credit to Emacs From Scratch for this one!"
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

(require 'use-package-ensure) ;; Load use-package-always-ensure
    (setq use-package-always-ensure t) ;; Always ensures that a package is installed
    (setq package-archives '(("melpa" . "https://melpa.org/packages/") ;; Sets default package repositories
                             ("org" . "https://orgmode.org/elpa/")
                             ("elpa" . "https://elpa.gnu.org/packages/")
))
                             ;;("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; For Eat Terminal

(add-to-list 'exec-path "/opt/homebrew/bin")
(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x)) ;; Only needed on macOS/NS
  :config
  (setq exec-path-from-shell-variables '("PATH" "OPENROUTER_API_KEY"))
  (exec-path-from-shell-initialize))

(use-package evil
  :init ;; Execute code Before a package is loaded
  (evil-mode)
  :config ;; Execute code After a package is loaded
  :custom ;; Customization of package custom variables
  (evil-want-keybinding nil)    ;; Disable evil bindings in other modes (It's not consistent and not good)
  (evil-want-C-u-scroll t)      ;; Set C-u to scroll up
  (evil-want-C-i-jump nil)      ;; Disables C-i jump
  (evil-undo-system 'undo-redo) ;; C-r to redo
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  ;; Unmap keys in 'evil-maps. If not done, org-return-follows-link will not work
  :bind (:map evil-motion-state-map
              ("SPC" . nil)
              ("RET" . nil)
              ("TAB" . nil)))
(use-package evil-collection
  :after evil
  :config
  ;; Setting where to use evil-collection
  (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult))
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup)
  ;; Set up 'SPC' as the leader key
  (general-create-definer start/leader-keys
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC"           ;; Set leader key
    :global-prefix "C-SPC") ;; Set global leader key

  (start/leader-keys
    "." '(find-file :wk "Find file")
    "TAB" '(comment-line :wk "Comment lines")
    "p" '(projectile-command-map :wk "Projectile command map")
	":" '(execute-extended-command :wk "M-x"))

  (start/leader-keys
    "f" '(:ignore t :wk "Find")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
    "f r" '(consult-recent-file :wk "Recent files")
    "f f" '(consult-fd :wk "Fd search for files")
    "f g" '(consult-ripgrep :wk "Ripgrep search in files")
    "f l" '(consult-line :wk "Find line")
    "f s" '(save-buffer :wk "File save")
    "f i" '(consult-imenu :wk "Imenu buffer locations"))

  (start/leader-keys
    "b" '(:ignore t :wk "Buffer Bookmarks")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b k" '(kill-current-buffer :wk "Kill this buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b j" '(consult-bookmark :wk "Bookmark jump"))

  (start/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d v" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current"))

  (start/leader-keys
    "w" '(:ignore t :wk "Window")
    "w d" '(evil-window-delete :wk "Window delete")
	"w D" '(delete-other-windows :wk "Delete other windows")
    "w s" '(evil-window-split :wk "Window split")
    "w v" '(evil-window-vsplit :wk "Window split vertical")
    "w h" '(evil-window-left :wk "Window left")
    "w H" '(evil-window-move-far-left :wk "Window move left")
    "w l" '(evil-window-right :wk "Window right")
    "w L" '(evil-window-move-far-right :wk "Window move right")
    "w k" '(evil-window-up :wk "Window up")
    "w K" '(evil-window-move-very-top :wk "Window move up")
    "w j" '(evil-window-down :wk "Window down")
    "w J" '(evil-window-move-very-bottom :wk "Window move down"))

  (start/leader-keys
    "g" '(:ignore t :wk "Git")
    "g g" '(magit-status :wk "Magit status"))

  (start/leader-keys
    "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
    "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "h r" '((lambda () (interactive)
              (load-file "~/.config/emacs/init.el"))
            :wk "Reload Emacs config"))

  (start/leader-keys
    "s" '(:ignore t :wk "Show")
    "s v" '(vterm :wk "vterm"))

  (start/leader-keys
    "o" '(:ignore t :wk "Org")
    "o a" '(org-agenda :wk "Org agenda")
	"o c" '(org-capture :wk "Org capture"))

  (start/leader-keys
    "q" '(:ignore t :wk "Quit")
    "q q" '(evil-quit-all :wk "Quit emacs"))

  (start/leader-keys
    "t" '(:ignore t :wk "Toggle")
	"t b" '(my/toggle-big-font :wk "Toggle big font")
    "t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers"))
  (start/leader-keys
	"c" '(:ignore t :wk "Code")
	"c a" '(lsp-execute-code-action :wk "Code actions")
	"c g r" '(lsp-find-references :wk "Find references")
	"c g d" '(lsp-goto-implementation :wk "Goto implementation"))
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   "gc" 'evilnc-comment-operator)
  )

(setq mac-right-option-modifier 'none)
(setq ns-right-option-modifier 'none)

(use-package avy
  :ensure t
  :bind
  (("M-s"     . avy-goto-char)
   ("M-g g"   . avy-goto-line)
   ("M-g M-g" . avy-goto-line)))

(use-package ace-window
  :ensure t
  :bind (("M-w" . ace-window)))

(use-package emacs
  :custom
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  ;;(inhibit-startup-screen t)  ;; Disable welcome screen
  
  (display-time-mode 1)

  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (electric-pair-mode t)      ;; Turns on automatic parens pairing

  (blink-cursor-mode nil)     ;; Don't blink cursor
  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  ;;(dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  ;;(recentf-mode t) ;; Enable recent file mode

  ;;(global-visual-line-mode t)           ;; Enable truncated lines
  (display-line-numbers-type 'relative) ;; Relative line numbers
  (global-display-line-numbers-mode t)  ;; Display line numbers

  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
  ;;(scroll-margin 8)

  (tab-width 4)

  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files
  :hook
  (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind (
         ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
         )
  ;; Fix general.el leader key not working instantly in messages buffer with evil mode
  :ghook ('after-init-hook
          (lambda (&rest _)
            (when-let ((messages-buffer (get-buffer "*Messages*")))
              (with-current-buffer messages-buffer
                (evil-normalize-keymaps))))
          nil nil t)
  )

;; Make projectile grep/symbol-at-point not include '=' at the end
(defun my/projectile-symbol-at-point ()
  "Like `projectile-symbol-at-point', but don't include '='."
  (let ((sym (thing-at-point 'symbol)))
    (when sym
      (replace-regexp-in-string "=$" "" sym))))

(advice-add 'projectile-symbol-at-point :override #'my/projectile-symbol-at-point)

;; (use-package gruvbox-theme
    ;;   :config
    ;;   (load-theme 'gruvbox-dark-medium t)) ;; We need to add t to trust this package
(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-to-list 'default-frame-alist '(alpha-background . 90)) ;; For all new frames henceforth

(set-face-attribute 'default nil
                    :font "JetBrains Mono" ;; Set your favorite type of font or download JetBrains Mono
                    :height 160
                    :weight 'medium)
;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.

;;(add-to-list 'default-frame-alist '(font . "JetBrains Mono")) ;; Set your favorite font
(setq-default line-spacing 0.12)

(defvar my/default-font "JetBrains Mono")
(defvar my/default-font-size 160)
(defvar my/big-font-size 220) ;; Or however big you want it

(defun my/set-font-size (size)
  "Set the default font size to SIZE (in 1/10 pt)."
  (interactive "nFont size (10x pt): ")
  (set-face-attribute 'default nil :font my/default-font :height size))

(defvar my/big-font-enabled nil)

(defun my/toggle-big-font ()
  "Toggle between default and big font sizes."
  (interactive)
  (setq my/big-font-enabled (not my/big-font-enabled))
  (my/set-font-size (if my/big-font-enabled my/big-font-size my/default-font-size))
  (message "Font size set to %s" (if my/big-font-enabled "BIG" "normal")))

(use-package emacs
  :bind
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<C-wheel-up>" . text-scale-increase)
  ("<C-wheel-down>" . text-scale-decrease))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)     ;; Sets modeline height
  (doom-modeline-bar-width 5)   ;; Sets right bar width
  (doom-modeline-persp-name t)  ;; Adds perspective name to modeline
  (doom-modeline-persp-icon t)) ;; Adds folder icon next to persp name

(add-to-list 'load-path "~/.config/emacs-extra/mcp.el") ;; adjust if needed

(use-package mcp
  :ensure nil  ;; don't pull from MELPA
  :config
  (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

(use-package gptel
  :ensure t
  :config
  (setq gptel-api-key #'(lambda () (getenv "OPENROUTER_API_KEY")))

  (setq gptel-backend
        (gptel-make-openai
         "OpenRouter"
         :host "openrouter.ai"
         :endpoint "/api/v1/chat/completions"
         :stream t
         :key gptel-api-key
         :models '(google/gemini-2.0-flash-001
				   google/gemini-2.5-flash-preview-05-20
                   ;; other models optional
                   )))

  (setq gptel-default-backend gptel-backend
        gptel-model 'google/gemini-2.0-flash-001
        gptel-default-mode 'org-mode)

  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  (require 'gptel-integrations)
  (setq mcp-hub-servers nil)
  (setq mcp-hub-servers 
		'(("github" . (:command "docker"
								))))

  (setq mcp-hub-servers 
		`(("github" . (:command "docker"
								:args ("run" "--rm" "-i" "-e" "GITHUB_PERSONAL_ACCESS_TOKEN" "ghcr.io/github/github-mcp-server") 
								:env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(getenv "GITHUB_MCP_PAT"))))))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
										; defun my-get-openrouter-api-key yourself elsewhere for security reasons

  (setenv "OPENROUTER_API_KEY" (getenv "OPENROUTER_API_KEY"))
  :custom
										; See the Configuration section below
  (aidermacs-default-chat-mode 'ask)
  (aidermacs-default-model "sonnet"))

(setq aidermacs-global-read-only-files '("~/.aider/AI_RULES.md"))
(setq aidermacs-project-read-only-files '("README.md"))

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (copilot-mode 1))
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t) ;; Interactive run dialog when running projects inside emacs (like giving input)
  (projectile-switch-project-action #'projectile-dired) ;; Open dired when switching to a project
  (projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))) ;; . 1 means only search the first subdirectory level for projects
;; Use Bookmarks for smaller, not standard projects

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; (use-package company
;;   :hook (lsp-mode . company-mode)
;;   :config
;;   (setq company-minimum-prefix-length 1
;;         company-idle-delay 0.0))  ;; show completions immediately

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l") 
  :hook ((typescript-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-completion-provider :none))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package which-key
  :config
  (which-key-mode))

;; Disable other TS/JS language servers to avoid conflicts
(setq lsp-disabled-clients '(ts-ls deno-ls))

;; Register vtsls as the preferred LSP for TS/JS
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("vtsls" "--stdio"))
    :major-modes '(typescript-mode tsx-ts-mode typescript-ts-mode js-mode js-ts-mode)
    :server-id 'vtsls
    :priority 3
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         `(:typescript (:format (:enable t))
                           :javascript (:format (:enable t))
                           :vtsls (:experimental (:completion (:enableServerSideFuzzyMatch t))))))))))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package evil-nerd-commenter
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 't)    ; or 't for silent
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package typescript-ts-mode 
  :ensure t
  :hook (typescript-ts-mode . lsp-deferred)
  )
(add-hook 'typescript-ts-mode-hook #'lsp-deferred)
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)

(use-package dotenv-mode
  :ensure t
  :mode (("\\.env\\..*\\'" . dotenv-mode)
         ("\\.env\\'" . dotenv-mode)))

(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.
  (org-agenda-files (append
    				 (directory-files-recursively "~/org" "\\.org$")
    				 (directory-files-recursively "~/notes" "\\.org$")))

  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\n  %U\n  %a")
     ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
      "* %?\n  %U\n  %a")
     ("i" "Inbox" entry (file "~/org/inbox.org")
      "* %?\nEntered on %U\n  %i\n  %a")))
  (org-log-into-drawer t)
  (org-clock-into-drawer t)
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(p)" "|" "DONE(d)")
     (sequence "WAIT(w)" "HOLD(h)" "|" "CANCELLED(c)")))
  
  (org-log-done 'time) ;; Log timestamp when marked DONE
  (org-log-into-drawer t) ;; Store logs in :LOGBOOK: drawer

  (org-confirm-babel-evaluate nil)
  (org-pretty-entities t) ;; Replace things like \alpha with α
  (org-hide-emphasis-markers t) ;; Hide *bold* markers visually


  :hook
  (org-mode . org-indent-mode) ;; Indent text
  ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
  ;; Otherwise, org-tempo is broken when you try to <s TAB...
  ;;(org-mode . (lambda ()
  ;;              (setq-local electric-pair-inhibit-predicate
  ;;                          `(lambda (c)
  ;;                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  )

(use-package ob-typescript
  :ensure t
  :after org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (js . t)
   (typescript . t)))

(use-package htmlize
  :ensure t)

(add-to-list 'load-path "~/.config/emacs-extra/org-reveal")
(require 'ox-reveal)

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(setq org-superstar-remove-leading-stars t)
(setq org-superstar-special-todo-items t)
(setq org-superstar-headline-bullets-list '("●" "○" "◆" "◇" "▸"))
(setq org-superstar-leading-bullet ?\s) ;; Use space instead of . or anything
(setq org-hide-leading-stars nil) ;; Optional, shows all stars if you want
(setq org-indent-mode-turns-on-org-indent nil) ;; 🔥 the important bit

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-tempo
  :ensure nil
  :after org)

(use-package org-roam
    :ensure t
    :custom
    (org-roam-directory (file-truename "~/org/roam/"))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ;; Dailies
           ("C-c n j" . org-roam-dailies-capture-today))
    :config
    ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-autosync-mode)
    ;; If using org-roam-protocol
    (require 'org-roam-protocol))
;; Create the .org-id-locations file if missing
(unless (file-exists-p org-id-locations-file)
  (org-id-update-id-locations (directory-files-recursively org-roam-directory "\\.org$")))

(use-package vterm
    :ensure t)

;; (defvar nba--ghost-overlay nil "Overlay for NBA ghost text preview.")

;; (defun nba/first-n (s n)
;;   "Return the first N characters of string S, or \"\" if S is nil."
;;   (if (and s (stringp s))
;;       (substring s 0 (min n (length s)))
;;     ""))

;; (defun nba/capture-edit-context ()
;;   "Capture relevant context for next best action, focusing on buffer."
;;   (let ((buf-name (or (buffer-name) "unnamed-buffer"))
;;         (mode (or major-mode 'fundamental-mode))
;;         (contents (or (buffer-substring-no-properties (point-min) (point-max)) "")))
;;     (message "[nba/capture-edit-context] buf-name: %S, mode: %S, first-30: %S"
;;              buf-name mode (nba/first-n contents 30))
;;     (list :buffer buf-name :mode mode :contents contents)))

;; (defun nba/build-prompt (context)
;;   "Prompt LLM for just the updated code, nothing else."
;;   (let* ((buf (plist-get context :buffer))
;;          (mode (plist-get context :mode))
;;          (contents (plist-get context :contents)))
;;     (message "[nba/build-prompt] buf: %S, mode: %S, first-30: %S"
;;              buf mode (nba/first-n contents 30))
;;     (format
;;      "You are an expert %s developer. I am working in the buffer \"%s\" (major-mode: %s). Here are the current contents:\n\n%s

;; Given my recent changes, determine which *single* function or interface must be updated. Return ONLY the entire, updated code for that symbol, as plain text—no explanations, no markdown, no triple backticks, no comments, no headers, no extra output. Only the complete code I should paste in, as it would appear in the file.

;; For example, if the 'createUser' function must change, return ONLY:

;; export function createUser(name: string, email: string, age: number): User {
;;   return {
;;     name,
;;     email,
;;     age,
;;   };
;; }

;; Do NOT return any explanations, comments, markdown, or other symbols. If more than one symbol could be changed, pick the most important one and only return that."
;;      (if (string-match "typescript" (symbol-name mode)) "TypeScript" "programming language")
;;      buf mode contents))
;; )

;; (defun nba/get-symbol-name (code)
;;   "Extract the symbol name (function or interface) from CODE."
;;   (cond
;;    ;; function foo(...
;;    ((string-match "function[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*(" code)
;;     (match-string 1 code))
;;    ;; interface Foo {
;;    ((string-match "interface[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*{" code)
;;     (match-string 1 code))
;;    (t nil)))

;; (defun nba/show-ghost-text (symbol-name code)
;;   "Show CODE as ghost text replacing SYMBOL-NAME in the current buffer."
;;   (nba/remove-ghost-text)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((pattern (concat
;;                     "\\_<\\(function\\|interface\\)\\_>[ \t\n]+"
;;                     (regexp-quote symbol-name)
;;                     "\\b")))
;;       (if (re-search-forward pattern nil t)
;;           (let ((start (match-beginning 0)))
;;             (when (re-search-forward "^[}]\\s-*$" nil t)
;;               (let ((end (point)))
;;                 (setq nba--ghost-overlay
;;                       (make-overlay start end (current-buffer) nil t))
;;                 (overlay-put nba--ghost-overlay 'after-string
;;                              (propertize code 'face '(:foreground "grey50")))
;;                 (overlay-put nba--ghost-overlay 'nba-ghost t)
;;                 (message "Ghost text preview shown. Press C-c C-a to accept, C-c C-d to dismiss."))))
;;         (message "Could not find symbol %s in buffer" symbol-name)))))

;; (defun nba/remove-ghost-text ()
;;   "Remove any NBA ghost text overlay."
;;   (when (and nba--ghost-overlay (overlayp nba--ghost-overlay))
;;     (delete-overlay nba--ghost-overlay)
;;     (setq nba--ghost-overlay nil)))

;; (defun nba/accept-ghost-text ()
;;   "Accept and insert the current ghost text."
;;   (interactive)
;;   (when (and nba--ghost-overlay (overlayp nba--ghost-overlay))
;;     (let ((start (overlay-start nba--ghost-overlay))
;;           (end (overlay-end nba--ghost-overlay))
;;           (code (overlay-get nba--ghost-overlay 'after-string)))
;;       (save-excursion
;;         (goto-char start)
;;         (delete-region start end)
;;         (insert code)))
;;     (nba/remove-ghost-text)
;;     (message "Code inserted!")))

;; (defun nba/dismiss-ghost-text ()
;;   "Dismiss the current ghost text overlay."
;;   (interactive)
;;   (nba/remove-ghost-text)
;;   (message "Ghost text dismissed."))

;; ;; Keybindings (global, but you can scope them to typescript-mode if you prefer)
;; (define-key global-map (kbd "C-c C-a") 'nba/accept-ghost-text)
;; (define-key global-map (kbd "C-c C-d") 'nba/dismiss-ghost-text)

;; (defun nba/display-suggestions (response info)
;;   "Show LLM code as ghost text in the current buffer."
;;   (let* ((code (string-trim response))
;;          (symbol-name (nba/get-symbol-name code)))
;;     (message "[nba/display-suggestions] symbol: %S" symbol-name)
;;     (when symbol-name
;;       (nba/show-ghost-text symbol-name code))))

;; (defun nba/send-to-llm (context)
;;   "Send CONTEXT to LLM and return suggestions."
;;   (message "[nba/send-to-llm] context: %S" context)
;;   (let ((prompt (nba/build-prompt context)))
;;     (message "[nba/send-to-llm] prompt first-80: %S"
;;              (nba/first-n prompt 80))
;;     (gptel-request prompt :callback #'nba/display-suggestions)))

;; (defun nba/next-best-action ()
;;   "Ask LLM for next best action after current edit."
;;   (interactive)
;;   (message "[nba/next-best-action] called from buffer: %S, mode: %S"
;;            (buffer-name) major-mode)
;;   (nba/send-to-llm (nba/capture-edit-context)))

(defvar nba--ghost-overlay nil "Overlay for NBA ghost text preview.")

(defun nba/first-n (s n)
  "Return the first N characters of string S, or \"\" if S is nil."
  (if (and s (stringp s))
      (substring s 0 (min n (length s)))
    ""))

;;;; === Section: Context Collection ===

(defun nba/git-diff-current-file ()
  "Get the git diff for the current buffer file."
  (when-let ((file (buffer-file-name)))
    (with-temp-buffer
      (call-process "git" nil t nil "diff" file)
      (buffer-string))))

(defun nba/lsp-diagnostics ()
  "Collect LSP diagnostics (errors, warnings) for current buffer."
  (when (and (bound-and-true-p lsp-mode)
             (fboundp 'lsp-diagnostics))
    (let* ((file (or (buffer-file-name) (buffer-name)))
           (diags-by-file (lsp-diagnostics))
           (file-diags (gethash file diags-by-file)))
      (when file-diags
        (mapconcat
         (lambda (diag)
           (format "%s [%s]: %s"
                   (gethash "source" diag)
                   (gethash "severity" diag)
                   (gethash "message" diag)))
         file-diags "\n")))))

(defun nba/flycheck-issues ()
  "Collect Flycheck issues for current buffer."
  (when (bound-and-true-p flycheck-mode)
    (let ((errors (flycheck-overlay-errors-in (point-min) (point-max))))
      (mapconcat #'flycheck-error-message errors "\n"))))

(defun nba/treesit-summary ()
  "Summarize top-level symbols in buffer using Tree-sitter."
  (when (and (fboundp 'treesit-ready-p)
             (treesit-ready-p major-mode))
    (let* ((root (treesit-buffer-root-node))
           (children (treesit-node-children root)))
      (mapconcat
       (lambda (node)
         (format "%s: %s"
                 (treesit-node-type node)
                 (string-trim (treesit-node-text node))))
       children "\n"))))

(defun nba/collect-context ()
  "Collect full context for LLM: git diff, LSP, treesit, etc."
  (let* ((diff (nba/git-diff-current-file))
         (lsp (or (nba/lsp-diagnostics) (nba/flycheck-issues)))
         (ast (nba/treesit-summary))
         (buf (buffer-name))
         (mode major-mode))
    (list :buffer buf :mode mode
          :diff diff
          :lsp lsp
          :ast ast)))

;;;; === Section: Prompt Construction (Generic) ===

(defun nba/build-prompt (context)
  "Builds a generic LLM prompt for code action suggestions (pure code output)."
  (let ((buf (plist-get context :buffer))
        (mode (plist-get context :mode))
        (diff (plist-get context :diff))
        (lsp (plist-get context :lsp))
        (ast (plist-get context :ast)))
    (format
     "You are an expert %s developer. I am working in the buffer \"%s\" (major-mode: %s).

Here is the context for your suggestion:
---
*Git diff for this file:*
%s

*Diagnostics (errors, warnings):*
%s

*File structure summary:*
%s
---

Given ONLY this information, determine what single top-level code element (such as a function, class, type, interface, or variable) most obviously needs updating or completion in this file.

Return ONLY the full, updated code for that element, exactly as it should appear in the file. DO NOT include explanations, comments, markdown, triple backticks, code fences, or any extra output—only the code. If more than one symbol could be updated, pick the most immediately relevant to the last change.

**Output format example (for a function):**

export function example(...) {
  // ...
}

(But the actual output must be only the code, for the most relevant symbol, nothing else.)"
     (if (string-match "typescript" (symbol-name mode)) "TypeScript" "programming language")
     buf mode (or diff "none") (or lsp "none") (or ast "none"))))

;;;; === Section: Ghost Text Overlay ===

(defun nba/get-symbol-name (code)
  "Extract the symbol name from CODE for function, interface, class, type, or variable."
  (or
   (when (string-match "function[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*(" code)
     (match-string 1 code))
   (when (string-match "interface[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*{" code)
     (match-string 1 code))
   (when (string-match "class[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*" code)
     (match-string 1 code))
   (when (string-match "type[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*=" code)
     (match-string 1 code))
   (when (string-match "\\_<\\(const\\|let\\|var\\)[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*[:=]" code)
     (match-string 2 code))))

(defun nba/clean-code-output (response)
  "Extracts pure code from RESPONSE, removing any markdown code fences."
  (let ((trimmed (string-trim response)))
    (if (string-match "```[a-zA-Z0-9]*\n\\([\\s\\S]+?\\)```" trimmed)
        (string-trim (match-string 1 trimmed))
      trimmed)))

(defun nba/show-ghost-text (symbol-name code)
  "Show CODE as ghost text replacing SYMBOL-NAME in the current buffer."
  (nba/remove-ghost-text)
  (save-excursion
    (goto-char (point-min))
    (let* ((pattern (concat
                     "\\_<\\(function\\|interface\\|class\\|type\\|const\\|let\\|var\\)\\_>[ \t\n]+"
                     (regexp-quote symbol-name)
                     "\\b"))
           (found (re-search-forward pattern nil t)))
      (if found
          (let ((start (match-beginning 0)))
            ;; For types/interfaces/classes/functions: match block or assignment
            (cond
             ;; function/class/interface block
             ((looking-at "[^{=]*{")
              (goto-char (match-end 0))
              (when (re-search-forward "^[}]\\s-*$" nil t)
                (let ((end (point)))
                  (setq nba--ghost-overlay
                        (make-overlay start end (current-buffer) nil t))
                  (overlay-put nba--ghost-overlay 'after-string
                               (propertize code 'face '(:foreground "grey50")))
                  (overlay-put nba--ghost-overlay 'nba-ghost t)
                  (message "Ghost text preview shown. Press C-c C-a to accept, C-c C-d to dismiss."))))
             ;; type/const/let/var assignment
             ((looking-at "[^=]*=")
              (goto-char (match-end 0))
              (end-of-line)
              (let ((end (point)))
                (setq nba--ghost-overlay
                      (make-overlay start end (current-buffer) nil t))
                (overlay-put nba--ghost-overlay 'after-string
                             (propertize code 'face '(:foreground "grey50")))
                (overlay-put nba--ghost-overlay 'nba-ghost t)
                (message "Ghost text preview shown. Press C-c C-a to accept, C-c C-d to dismiss."))))
        (message "Could not find symbol %s in buffer" symbol-name))))))

(defun nba/remove-ghost-text ()
  "Remove any NBA ghost text overlay."
  (when (and nba--ghost-overlay (overlayp nba--ghost-overlay))
    (delete-overlay nba--ghost-overlay)
    (setq nba--ghost-overlay nil)))

(defun nba/accept-ghost-text ()
  "Accept and insert the current ghost text."
  (interactive)
  (when (and nba--ghost-overlay (overlayp nba--ghost-overlay))
    (let ((start (overlay-start nba--ghost-overlay))
          (end (overlay-end nba--ghost-overlay))
          (code (overlay-get nba--ghost-overlay 'after-string)))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert code)))
    (nba/remove-ghost-text)
    (message "Code inserted!")))

(defun nba/dismiss-ghost-text ()
  "Dismiss the current ghost text overlay."
  (interactive)
  (nba/remove-ghost-text)
  (message "Ghost text dismissed."))

;; Keybindings (global, but you can scope them to typescript-mode if you prefer)
(define-key global-map (kbd "C-c C-a") 'nba/accept-ghost-text)
(define-key global-map (kbd "C-c C-d") 'nba/dismiss-ghost-text)

;;;; === Section: LLM Integration ===

(defun nba/display-suggestions (response info)
  "Show LLM code as ghost text in the current buffer."
  (let* ((code (nba/clean-code-output response))
         (symbol-name (nba/get-symbol-name code)))
    (if symbol-name
        (nba/show-ghost-text symbol-name code)
      (message "Could not determine symbol name from LLM output!"))))

(defun nba/send-to-llm (context)
  "Send CONTEXT to LLM and return suggestions."
  (let ((prompt (nba/build-prompt context)))
    (gptel-request prompt :callback #'nba/display-suggestions)))

;;;; === Section: Trigger ===

(defun nba/next-best-action ()
  "Ask LLM for next best action after current edit."
  (interactive)
  (nba/send-to-llm (nba/collect-context)))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (require 'start-multiFileExample)

;; (start/hello)

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package magit
  ;; :custom (magit-diff-refine-hunk (quote all)) ;; Shows inline diff
  :commands magit-status)

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package corfu-terminal
;;   :ensure t
;;   :after corfu
;;   :config
;;   (unless (display-graphic-p)
;;     (corfu-terminal-mode +1)))

(use-package cape
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; The functions that are added later will be the first in the list

  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keyword/Snipet completion

  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-to-list 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-to-list 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-to-list 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
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
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
   ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
   ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
   ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
   ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
   ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure nil ;; Don't install which-key because it's now built-in
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display, because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(use-package elfeed
  :ensure t
  :bind (("C-x w" . elfeed))
  :custom
  (elfeed-feeds
   '(
     ;; Dev & Emacs
     ("https://planet.emacslife.com/atom.xml" emacs)
     ("https://nullprogram.com/feed/" programming)
     ("https://xenodium.com/rss.xml" emacs personal)
     ;; Tech/News
     ("https://lobste.rs/rss" tech)
     ("https://hnrss.org/frontpage" hackernews)
     )))

(use-package flycheck-vale
  :ensure t
  :after flycheck
  :config
  (flycheck-vale-setup))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
