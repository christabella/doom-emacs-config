;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Christabella Irwanto"
      user-mail-address "christabella.irwanto@relexsolutions.com")

;; https://github.com/edwardtufte/et-book
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 16 :style "RomanLF")
      display-line-numbers-type nil
      confirm-kill-emacs nil)

;; Alternatives: doom-laserwave, doom-dracula, doom-moonlight
(setq doom-theme 'doom-fairy-floss)

;; Autofill comments
(setq comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook #'auto-fill-mode)


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
  (map! "s-E" #'emojify-insert-emoji))

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
   "C-c R" #'crux-rename-file-and-buffer))


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

(setq org-roam-directory "~/roam"
      org-roam-db-location "~/roam/org-roam.db"
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
  :hook (text-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))  ;; Let height be overridden by doom-font

(after! org
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
  ;; From roam/wiki/key_codebases.org
  (setq org-link-abbrev-alist
        '(("plan-mcp"          . "https://gitlab.relexsolutions.com/system-management/plan-sre/plan-mcp/-/blob/main/%s")
          ("planning-cloud"    . "https://gitlab.relexsolutions.com/DevHEL/planning-cloud/-/blob/master/%s")
          ("relexgpt-api"      . "https://gitlab.relexsolutions.com/labs-engineering/relex-gpt/relexgpt-api/-/blob/main/%s")
          ("relexgpt-frontend" . "https://gitlab.relexsolutions.com/labs-engineering/relex-gpt/relexgpt-frontend/-/blob/master/%s")))
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
        org-fontify-done-headline t))

(use-package! org-journal
  :config
  (map!
   "C-c j" #'org-journal-new-entry
   "s-j" #'org-journal-new-entry
   "s-J" #'org-journal-open-current-journal-file
   "C-c C-p" #'org-journal-previous-entry
   "C-c C-n" #'org-journal-next-entry))

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
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-directory "daily/")   ;; -> ~/roam/daily (work capture inbox)
  (map! "C-c z" #'org-roam-node-find          ;; Zettel: find/create a node
        "C-c r" #'org-roam-node-insert        ;; Ref: link to another node
        "C-c d"   #'org-roam-dailies-capture-today) ;; work brain-dump (s-j stays personal)
  ;; Simple start: one "note" template -> wiki/, tagged :work: by default.
  ;; Add :general:/:person:/:project:/:moc: by editing #+filetags in the node itself.
  (setq org-roam-capture-templates
        '(("n" "note" plain "%?"
           :target (file+head "wiki/${slug}.org"
                              "#+title: ${title}\n#+filetags: :work:\n")
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "work daily" entry "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:work:\n")))))

(use-package! treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package! emacsql-sqlite3)
(setq org-roam-database-connector 'sqlite3)

(unless (server-running-p)
  (server-start))
