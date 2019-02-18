;;; config-windows.el --- Windows management and navigation

;;

;;; Commentary:
;;

;;; Code:
(require 'use-config)

(use-package ace-window
  :ensure t
  :defer t
  :commands (aw-window-list)
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
        aw-scope 'frame))

(use-package windmove)

(use-package winner
  :defer t
  :init
  (after-init #'winner-mode))

(use-package zygospore
  :ensure t
  :defer t)

(use-package buffer-move
  :ensure t
  :defer t)

(use-package windresize
  :ensure t
  :defer t
  :commands (windresize-left windresize-right windresize-up windresize-down)
  :config
  (validate-setq windresize-default-increment 5))

;;;###autoload
(defun memes/switch-to-last-window ()
  "Switch to the most recently used window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

;;;###autoload
(defun memes/window-split-toggle ()
  "Toggle between horizontal and vertical split."
  (interactive)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (select-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

;; Popwin to help wrangle informative buffers
(use-package popwin
  :ensure t
  :defer t
  :config
  (progn
    (popwin-mode 1)))

(provide 'config-windows)
;;; config-windows.el ends here
