;; Define a custom colour scheme
;; $Id: colour-scheme.el 1 2007-10-18 23:15:33Z memes $

(progn
  (custom-set-faces
   '(default ((t (:stipple nil :background "Beige" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal))))
   '(bold ((t (:bold t :background "grey40" :foreground "yellow"))))
   '(bold-italic ((t (:italic t :bold t :foreground "yellow green"))))
   '(custom-button-face ((t (nil))))
   '(custom-changed-face ((t (:background "blue" :foreground "white"))))
   '(custom-documentation-face ((t (nil))))
   '(custom-face-tag-face ((t (:underline t))))
   '(custom-group-tag-face ((t (:underline t :foreground "blue"))))
   '(custom-group-tag-face-1 ((t (:underline t :foreground "red"))))
   '(custom-invalid-face ((t (:background "red" :foreground "yellow"))))
   '(custom-modified-face ((t (:background "blue" :foreground "white"))))
   '(custom-rogue-face ((t (:background "black" :foreground "pink"))))
   '(custom-saved-face ((t (:underline t))))
   '(custom-set-face ((t (:background "white" :foreground "blue"))))
   '(custom-state-face ((t (:foreground "dark green"))))
   '(custom-variable-button-face ((t (:underline t :bold t))))
   '(custom-variable-tag-face ((t (:underline t :foreground "blue"))))
   '(font-lock-builtin-face ((t (:foreground "Orchid"))))
   '(font-lock-comment-face ((t (:foreground "MediumBlue"))))
   '(font-lock-constant-face ((t (:foreground "CadetBlue"))))
   '(font-lock-function-name-face ((t (:foreground "MediumSlateBlue"))))
   '(font-lock-keyword-face ((t (:foreground "#80a0ff"))))
   '(font-lock-string-face ((t (:foreground "red"))))
   '(font-lock-type-face ((t (:foreground "ForestGreen"))))
   '(font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
   '(font-lock-warning-face ((t (:bold t :foreground "Red"))))
   '(highlight ((t (:background "PaleGreen" :foreground "black"))))
   '(italic ((t (:italic t :foreground "yellow3"))))
   '(list-matching-lines-face ((t (bold))))
   '(message-cited-text-face ((t (:foreground "red"))))
   '(message-header-cc-face ((t (:foreground "MidnightBlue"))))
   '(message-header-name-face ((t (:foreground "cornflower blue"))))
   '(message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))
   '(message-header-other-face ((t (:foreground "steel blue"))))
   '(message-header-subject-face ((t (:bold t :foreground "navy blue"))))
   '(message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))
   '(message-header-xheader-face ((t (:foreground "blue"))))
   '(message-separator-face ((t (:foreground "brown"))))
   '(modeline ((t (:background "wheat" :foreground "DarkOliveGreen"))))
   '(modeline-buffer-id ((t (:background "wheat" :foreground "DarkOliveGreen"))))
   '(modeline-mousable ((t (:background "wheat" :foreground "DarkOliveGreen"))))
   '(modeline-mousable-minor-mode ((t (:background "wheat" :foreground "DarkOliveGreen"))))
   '(nil ((t (nil))))
   '(region ((t (:background "dark cyan" :foreground "cyan"))))
   '(rmail-highlight-face ((t (font-lock-function-name-face))))
   '(secondary-selection ((t (:background "Turquoise" :foreground "black"))))
   '(show-paren-match-face ((t (:background "turquoise"))))
   '(show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
   '(underline ((t (:underline t))))
   '(viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))
   '(viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))
   '(viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))
   '(viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))
   '(viper-search-face ((t (:background "khaki" :foreground "Black"))))
   '(widget-button-face ((t (:bold t))))
   '(widget-button-pressed-face ((t (:foreground "red"))))
   '(widget-documentation-face ((t (:foreground "dark green"))))
   '(widget-field-face ((t (:background "gray85"))))
   '(widget-inactive-face ((t (:foreground "dim gray"))))
   '(widget-single-line-field-face ((t (:background "gray85")))))
  (setq initial-frame-alist
	(cons 
	 '(foreground-color  . "Black")
	 initial-frame-alist))
  (setq initial-frame-alist
	(cons 
	 '(background-color  . "Beige")
	 initial-frame-alist))
  (setq initial-frame-alist
	(cons 
	 '(background-mode . light)
	 initial-frame-alist))
  (setq initial-frame-alist
	(cons 
	 '(cursor-color      . "Maroon")
	 initial-frame-alist))
  (setq initial-frame-alist
	(cons 
	 '(mouse-color      . "black")
	 initial-frame-alist))
  (setq initial-frame-alist
	(cons 
	 '(border-color      . "black")
	     initial-frame-alist))
  (setq default-frame-alist
	(cons 
	 '(foreground-color  . "Black")
	 default-frame-alist))
  (setq default-frame-alist
	(cons 
	 '(background-color  . "Beige")
	 default-frame-alist))
  (setq default-frame-alist
	(cons 
	 '(background-mode . light)
	 default-frame-alist))
  (setq default-frame-alist
	(cons 
	 '(cursor-color      . "Maroon")
	 default-frame-alist))
  (setq default-frame-alist
	(cons 
	 '(mouse-color      . "black")
	 default-frame-alist))
  (setq default-frame-alist
	(cons 
	 '(border-color      . "black")
	 default-frame-alist))
  (setq custom-set-face t)
  )

;; Colour syntax highlighting
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colours
       (setq font-lock-maximum-decoration t))
      )