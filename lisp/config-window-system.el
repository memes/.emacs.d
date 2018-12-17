;;; config-window-system.el --- Emacs configuration when used outside of a terminal

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(defcustom memes/frame-width 80
  "Initial frame width in characters."
  :type 'integer
  :group 'memes/window-system)

(defcustom memes/frame-height 50
  "Initial frame height in characters."
  :type 'integer
  :group 'memes/window-system)

(defvar memes/mono-fonts
  '("-*-Hack-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Consolas-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Menlo-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Courier New-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

(defun memes/mono-font ()
  "Return the first available font in `memes/mono-fonts`."
  (when (display-graphic-p)
    (cl-find-if #'memes/font-exists-p memes/mono-fonts)))

(defun memes/font-exists-p (font)
  "Existing 'FONT' predicate."
  (if (null (x-list-fonts font)) nil t))

(defun memes/primary-monitor-dimensions ()
  "Width and height of the primary monitor in pixels."
  (list (nth 3 (assq 'workarea (nth 0 (display-monitor-attributes-list))))
        (nth 4 (assq 'workarea (nth 0 (display-monitor-attributes-list))))))

(defun memes/frame-size (width-pct height-pct)
  "Compute a frame size as a list of (width, height) in pixels.
`WIDTH-PCT` and `HEIGHT-PCT` are integer percentages of the display size."
  (let* ((dim (memes/primary-monitor-dimensions))
         (width (car dim))
         (height (cadr dim)))
    (list
     (* width (/ width-pct 100.0))
     (* height (/ height-pct 100.0)))))

(defun memes/frame-origin (size)
  "Compute a list of (x, y) as the origin for a frame of `SIZE`."
  (let ((width (car size))
	(height (cadr size)))
    (list
     (* 0.5 (- (display-pixel-width) width))
     (* 0.5 (- (display-pixel-height) height)))))

(defun memes/config ()
  "Return a frame alist compatible with `initial-frame-alist`."
  (let ((config `(
                  ;; in pixels
                  (left . 0)
                  (top . 0)
                  ;; in chars
                  (width . ,memes/frame-width)
                  (height . ,memes/frame-height)
                  (menu-bar-lines . nil)
                  (tool-bar-lines . nil)
                  (vertical-scroll-bars . t)))
        (xfont (memes/mono-font)))
    ;; If the preferred font is found, use-it; but if not try to force the
    ;; first entry in preferred fonts list. Daemon mode has an initial frame
    ;; that is not graphic, so lookup of fonts will fail.
    (if xfont
        (append config `((font . ,xfont)))
      (append config `((font . ,(car memes/mono-fonts)))))))

;; Set initial/default configuration
(setq initial-frame-alist (memes/config))
(add-hook 'before-make-frame-hook (lambda ()
                                    (let ((config (memes/config)))
                                      (validate-setq default-frame-alist config))))

(use-package mwheel
  :defer t
  :if window-system
  :config
  (setq mouse-wheel-progressive-speed nil
	mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))))

(provide 'config-window-system)
;;; config-window-system.el ends here
