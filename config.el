;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. I like these themes:
;; (setq doom-theme 'doom-laserwave)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-moonlight)
(setq doom-theme 'doom-fairy-floss)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
  (map! "C-x g" #'magit-status
        "s-g" #'magit-status
        "s-G" #'magit-blame-addition
        "C-x M-g" #'magit-blame-addition))

;; Crux
(use-package! crux
  :defer t
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
  :defer t
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
      "M-q" #'other-window
      ;; Basic commands, not taken care of by `macos' in init.el...
      "s-x" #'kill-region
      "s-z" #'undo  ;; Good ol' Emacs fully-featured undo
      "s-/" #'comment-or-uncomment-region
      "M-s-/" #'comment-line
      ;; Ideally the above should work, but this Mac makes opt-/ into a ÷ symbol so...
      "M-s-÷" #'comment-line
      "s-<backspace>" #'sp-kill-whole-line
      "C-<backspace>" #'doom/backward-kill-to-bol-and-indent
      "C-S-<backspace>" #'fixup-whitespace
      "C-c j" #'counsel-projectile-git-grep
      "s-p" #'counsel-projectile-git-grep
      "s-P" #'counsel-projectile-switch-project
      "s-F" #'counsel-projectile-find-file
      )
