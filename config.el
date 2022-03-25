;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Richard Hao"
      user-mail-address "richard@digitalstar.tech")

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
;;(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; FiraCode Nerd Font https://github.com/tonsky/FiraCode
;;      ```
;;      brew tap homebrew/cask-fonts
;;      brew install --cask font-fira-code-nerd-font
;;      ```
;; ET Book https://edwardtufte.github.io/et-book/
;;      ```
;;      brew install --cask font-et-book
;;      '''
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org-notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; maximize/fullscreen Emacs on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;; :ui modeline
;; An evil mode indicator is redundant with cursor shape
(advice-add #'doom-modeline-segment--modals :override #'ignore)

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)
;; evil -> Restoring old substitution behavior on s/S
;; https://github.com/hlissner/doom-emacs/tree/master/modules/editor/evil#restoring-old-substitution-behavior-on-ss
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
;; evil -> Disabling cursor movement when exiting insert mode
;; Disabling cursor movement when exiting insert mode
(setq evil-move-cursor-back nil)

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

;; set up projectile search path
(setq projectile-project-search-path '("~/workspace/"))

;; doom banners
(let ((alternatives '("splash.png"
                      "lion-head.png"
                      "pilot-bust-go.svg"
                      "rails.png"
                      "ruby.png")))
  (setq fancy-splash-image
        (concat doom-private-dir "banners/"
                (nth (random (length alternatives)) alternatives))))
;; Hide the menu for as minimalistic a startup screen as possible.
;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; beacon
(beacon-mode 1)

;;; :app everywhere
(setq emacs-everywhere-major-mode-function #'org-mode)
(after! emacs-everywhere
  ;; Easier to match with a bspwm rule:
  ;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
  (setq emacs-everywhere-frame-name-format "emacs-anywhere")

  ;; The modeline is not useful to me in the popup window. It looks much nicer
  ;; to hide it.
  (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

  ;; Semi-center it over the target window, rather than at the cursor position
  ;; (which could be anywhere).
  (defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
    :override #'emacs-everywhere-set-frame-position
    (cl-destructuring-bind (x y width height)
        (emacs-everywhere-window-geometry window-info)
      (set-frame-position frame
                          (+ x (/ width 2) (- (/ width 2)))
                          (+ y (/ height 2))))))

;; Disable format for specific languages
;; https://github.com/lassik/emacs-format-all-the-code
;; https://github.com/hlissner/doom-emacs/tree/master/modules/editor/format
(setq-hook! 'ruby-mode-hook +format-with :none)


;; org-mode
(setq org-hide-emphasis-markers t)

;; Org mode
(add-hook 'org-mode-hook
        '(lambda ()
        ;; org-bullets
        (org-bullets-mode 1)

        ;;; Hide org block lines

        ;; Unset any previous customization for the background color
        (set-face-attribute 'org-block-begin-line nil :background 'unspecified)
        (set-face-attribute 'org-block-end-line nil :background 'unspecified)

        ;; Set the foreground color to the value of the background color
        (set-face-attribute 'org-block-begin-line nil
        :foreground (face-background 'org-block-begin-line nil 'default))
        (set-face-attribute 'org-block-end-line nil
        :foreground (face-background 'org-block-end-line nil 'default))
        ))

;;
;;; org-tree-slide
(defun efs/presentation-setup ()
  ;; Hide the mode line
  (hide-mode-line-mode 1)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Scale the text.  The next line is for basic scaling:
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

(defun efs/presentation-end ()
  ;; Show the mode line again
  (hide-mode-line-mode 0)

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  (text-scale-mode 0))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end)))
