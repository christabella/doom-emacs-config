;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Start in full screen by default
;; (toggle-frame-maximized)
;; ... but doing this breaks window resizing completely -_-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Christabella Irwanto"
      user-mail-address "christabella.irwanto@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka" :size 14)
      ;; Use pretty Edward Tufte (EtBembo) serif font for org-mode etc.
      ;; https://github.com/edwardtufte/et-book/blob/gh-pages/et-book/et-book-roman-line-figures/et-book-roman-line-figures.ttf
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 16)
      display-line-numbers-type nil
      confirm-kill-emacs nil
      +format-on-save-enabled-modes '(python-mode)
      )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. I like these themes:
(setq doom-theme 'doom-laserwave)
;; (setq doom-theme 'doom-fairy-floss)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-moonlight)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Autofill comments
(setq comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook #'auto-fill-mode)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("s-g" . magit-status)
         ("s-G" . magit-blame-addition)
         ("C-x M-g" . magit-blame-addition))
  :config
  ;; https://emacs.stackexchange.com/questions/28496/magit-status-always-split-vertically
  (setq split-width-threshold 200))

(use-package! nyan-mode
  :config
  (add-hook 'after-init-hook 'nyan-mode))

(use-package! emojify
  :init
  (add-hook 'after-init-hook 'global-emojify-mode)
  ;; Disable plain text emojis (no 'ascii' option)
  (setq emojify-emoji-styles '(github unicode))
  :config
  (map! "s-E" #'emojify-insert-emoji)
  )

;; Crux
(use-package! crux
  :config
  (map!
   "s-b" #'+ivy/switch-workspace-buffer
   "s-B" #'crux-switch-to-previous-buffer
   "s-d" #'crux-duplicate-current-line-or-region
   "s-D" #'crux-duplicate-and-comment-current-line-or-region
   "C-c o" #'crux-open-with
   "C-c D" #'crux-delete-file-and-buffer
   "C-c R" #'crux-rename-file-and-buffer
   ))
;; "C-<backspace>" #'crux-kill-line-backwards))


(use-package! multiple-cursors
  :config
  (map!
   "C->" #'mc/mark-next-like-this
   "C-<" #'mc/mark-previous-like-this
   ;; No need to select, will be function/chunk-specific.
   "C-c a" #'mc/mark-all-like-this-dwim
   ;; Needs selection, will be buffer-global.
   "C-c A" #'mc/mark-all-like-this)
  (map! :map mc/keymap
        ;; Make <return> insert a newline instead of disabling multiple-cursors-mode
        "<return>" #'newline-and-indent))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/"
      org-roam-directory "~/Dropbox/org/zettels"
      org-roam-db-location "~/org-roam.db"
      ;; Journal dir needs to be inside roam directory for backlinks to work as expected.
      org-journal-dir "~/Dropbox/org/zettels/journal"  ;; Daily "zettels"
      org-journal-carryover-items "TODO=\"TODO\"|TODO=\"DOING\""
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-prefix "#+TITLE: "  ;; Create new journal files with TITLE header.
      ;; Heading example: `Friday, 12 June 2020'
      org-journal-date-format "%A, %d %B %Y"
      ;; https://develop.spacemacs.org/layers/+emacs/org/README.html#org-journal-support
      org-journal-time-prefix "* " ;; Entries to start at first level heading
      org-ellipsis " ▼ "
      ;; Automatically add journal TODO's to agenda.
      org-journal-enable-agenda-integration t
      )

;; Hide backlinks buffer by default.
(setq +org-roam-open-buffer-on-find-file nil)

;; Disable smartparens-mode entirely in org-mode.
;; Hack to override M-right with org-metaright later.
(add-hook 'org-mode-hook #'turn-off-smartparens-mode)

(use-package! mixed-pitch
  :config
  (setq mixed-pitch-set-height t)  ;; Let height be overriden by my doom-font etc.
  :hook (text-mode . mixed-pitch-mode)
  )

;; Ensure code is run after the package (and Doom's defaults) are loaded with after!.
(after! org
  ;; :mode ("\\.org\\'" . org-mode)
  (map! :after (org)
        :map org-mode-map
        [C-return] #'org-insert-heading-respect-content
        [C-down] #'org-move-subtree-down
        [C-right] #'org-demote-subtree
        [C-left] #'org-promote-subtree
        [C-up] #'org-move-subtree-up
        [M-right] #'org-metaright
        [M-left] #'org-metaleft
        [M-up] #'drag-stuff-up
        [M-down] #'drag-stuff-down
        "M-n" #'outline-next-visible-heading
        "M-N" #'org-forward-heading-same-level
        "M-p" #'outline-previous-visible-heading
        "M-P" #'org-backward-heading-same-level
        "C-c u" #'org-cliplink
        )
  ;; By default: ("TODO" "PROJ" "STRT" "WAIT" "DONE" "KILL" "[ ]" "[-]" "[?]" "[X]" "FOUND" "READING" "CANCELED")
  (setq org-todo-keywords
        ;; https://orgmode.org/manual/Multiple-sets-in-one-file.html
        '((sequence "TODO" "DOING" "|" "DONE" "WON'T DO"))
        ;; Setting colours (faces) using M-x list-colors-display
        org-todo-keyword-faces
        '(("TODO" . "gold")
          ("DOING" . "pink")
          ("DONE" . "aquamarine")
          ("WON'T DO" . "medium purple"))
        org-fontify-done-headline t)
  ;; org-agenda-skip-scheduled-if-done t
  ;;        org-agenda-skip-deadline-if-done t
  ;;        org-todo-keywords-for-agenda '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
  )

(use-package! org-journal
  :config
  (map!
   "s-j" #'org-journal-new-entry
   "C-M-S-s-j" #'org-journal-new-entry
   "C-c C-b" #'org-journal-open-previous-entry
   "C-c C-f" #'org-journal-open-next-entry
   "C-M-S-s-k" #'org-journal-open-current-journal-file  ;; Today, or Kyō (今日)
   "s-J" #'org-journal-open-current-journal-file))

(use-package! org-download
  :commands
  org-download-dnd
  org-download-dnd-base64
  :init
  (map!
   :map org-mode-map
   "s-y" #'org-download-yank
   "s-Y" #'org-download-screenshot)
  :config
  (defun +org/org-download-method (link)
    (let* ((filename
            (file-name-nondirectory
             (car (url-path-and-query
                   (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           (dirname (concat "./images/"
                            (replace-regexp-in-string " " "_"
                                                      (downcase (file-name-base buffer-file-name)))))
           (filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  (setq org-download-method '+org/org-download-method
        org-download-image-org-width 600))

(use-package! treemacs
  :config
  (map! "C-M-S-s-t" #'treemacs))

;; Copy lines
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;; Override major modes
(map! "M-l" #'copy-line
      "M-q" #'ace-window
      ;; Basic commands, not taken care of by `macos' in init.el...
      "s-z" #'undo
      "s-x" #'kill-region
      "s-k" #'kill-current-buffer
      "s-/" #'comment-or-uncomment-region
      "s-t" #'doom/switch-to-scratch-buffer
      "M-s-/" #'comment-dwim
      ;; Ideally the above should work, but this Mac makes opt-/ into a ÷ symbol so...
      "M-s-÷" #'comment-dwim
      "s-<backspace>" #'sp-kill-whole-line
      "C-<backspace>" #'doom/backward-kill-to-bol-and-indent
      "C-S-<backspace>" #'fixup-whitespace
      "C-c B" #'browse-url-at-point
      "s-p" #'+default/search-project
      "s-P" #'+default/search-other-project
      "s-F" #'projectile-find-file
      "s-r" #'counsel-buffer-or-recentf
      "s-i" #'counsel-imenu
      "s-T" #'+fold/toggle
      "C-c p d" #'+default/discover-projects  ;; Add ~/repos/* to known projects.
      :map prog-mode-map
      "M-?" #'+lookup/documentation
      "M-r" #'+lookup/references
      "M-/" #'better-jumper-jump-forward  ;; M-, to jump-backward
      :map python-mode-map
      "M-n" #'python-nav-forward-defun
      "M-p" #'python-nav-backward-defun
      :map smartparens-mode-map
      "M-]" #'sp-forward-sexp
      "M-[" #'sp-backward-sexp
      "C-]" #'sp-rewrap-sexp
      :map flycheck-mode-map
      "S-s-<up>" #'flycheck-previous-error
      "S-s-<down>" #'flycheck-next-error)

(after! projectile
  (setq projectile-project-search-path '("~/repos/"))
  (add-to-list 'projectile-globally-ignored-directories "env")
  (add-to-list 'projectile-globally-ignored-directories ".venv"))

(after! org-roam
  (map! "C-c C-z" #'org-roam-node-find) ;; Open zettels drawer or file a new zettel.
  (map! "C-c C-r" #'org-roam-node-insert)  ;; Reference another zettel at point.
  :config
  ;; https://www.orgroam.com/manual.html#Template-Walkthrough
  (setq org-roam-capture-templates
        '(("l" "lit" plain "%?"
           :target (file+head "lit/${slug}.org"
                              "#+setupfile:./hugo_setup.org\n#+hugo_slug: ${slug}\n#+title: ${title}\n")
           :unnarrowed t)
        ("c" "concept" plain "%?"
           :target (file+head "concepts/${slug}.org"
                              "#+setupfile:./hugo_setup.org\n#+hugo_slug: ${slug}\n#+title: ${title}\n")
           :unnarrowed t)
        ("p" "personal" plain "%?"
           :target (file+head "personal/${slug}.org"
                              "#+title: ${title} (%<%Y-%m-%d>)\n")
           :unnarrowed t))
  ))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

;; For Python, use flycheck (flake8) for linting and +format (black) for formatting.
;; The `flycheck-flake8rc` variable is ".flake8rc" by default.
;; Otherwise, put this in a .dir-locals.el:
;;   ((python-mode . ((flycheck-flake8rc . "filename.ext"))))

(use-package! treemacs
  :config
  (map! "C-M-S-s-t" #'treemacs))
;; Only format with black if flake8 linter config exists.
(add-hook! 'python-mode-hook
  (flycheck-select-checker 'python-flake8)
  (unless (locate-dominating-file default-directory ".flake8")
    (format-all-mode -1))
  )

;; Only format if prettier config exists.
(add-hook! 'js2-mode-hook
  (unless (locate-dominating-file default-directory ".prettierrc")
    (format-all-mode -1)))

(use-package emacsql-sqlite3)
(setq org-roam-database-connector 'sqlite3)
