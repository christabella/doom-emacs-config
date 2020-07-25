;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Start in full screen by default
(toggle-frame-maximized)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
(setq display-line-numbers-type t)

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
  :config
  ;; https://emacs.stackexchange.com/questions/28496/magit-status-always-split-vertically
  (setq split-width-threshold 200)
  (map! "C-x g" #'magit-status
        "s-g" #'magit-status
        "s-G" #'magit-blame-addition
        "C-x M-g" #'magit-blame-addition))

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
      ;; Journal dir needs to be inside roam directory for backlinks to work as expected.
      org-journal-dir "~/Dropbox/org/zettels/journal"  ;; Daily "zettels"
      org-roam-db-location "~/org-roam.db"
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
(add-hook 'org-mode-hook #'variable-pitch-mode)

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
        ))

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
  :config
  (map!
   :map org-mode-map
   "s-y" #'org-download-yank
   "s-Y" #'org-download-screenshot
   ))

(use-package! treemacs
  :config
  (map! "C-M-S-s-t" #'treemacs))

(map! :map flycheck-mode-map
      "S-s-<up>" #'flycheck-previous-error
      "S-s-<down>" #'flycheck-next-error)

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
      "s-/" #'comment-or-uncomment-region
      "s-t" #'doom/switch-to-scratch-buffer
      "M-s-/" #'comment-dwim
      ;; Ideally the above should work, but this Mac makes opt-/ into a ÷ symbol so...
      "M-s-÷" #'comment-dwim
      "s-<backspace>" #'sp-kill-whole-line
      "C-<backspace>" #'doom/backward-kill-to-bol-and-indent
      "C-S-<backspace>" #'fixup-whitespace
      "C-c j" #'+default/search-project ;; TODO deprecate
      "s-p" #'+default/search-project
      "s-P" #'+default/search-other-project
      "s-F" #'projectile-find-file
      "s-r" #'counsel-buffer-or-recentf
      "C-c p d" #'+default/discover-projects  ;; Add ~/repos/* to known projects.
      )

(after! projectile
  (setq projectile-project-search-path '("~/repos/"))
  (add-to-list 'projectile-globally-ignored-directories "env")
  (add-to-list 'projectile-globally-ignored-directories ".venv"))

(after! org-roam
  (map! "C-M-S-s-z" #'org-roam-find-file) ;; Zettels
  (map! "C-M-S-s-b" #'org-roam)  ;; Backlinks buffer
  (map! "C-M-S-s-i" #'org-roam-insert)  ;; Insert link to zettel
  :config
  ;; https://www.orgroam.com/manual/Template-Walkthrough.html#Template-Walkthrough
  (setq org-roam-capture-templates
        '(("l" "lit" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("p" "personal" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "personal/${slug}"
           :head "#+title: ${title}"
           :unnarrowed t)
          ))
  )

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(add-hook! 'python-mode-hook
  (unless (locate-dominating-file default-directory ".flake8")
    (format-all-mode -1)))


(add-hook! 'js2-mode-hook
  (unless (locate-dominating-file default-directory ".prettierrc")
    (format-all-mode -1)))
