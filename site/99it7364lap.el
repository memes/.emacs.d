;; Settings specific to it7364lap
;; $Id: 99it7364lap.el 1 2007-10-18 23:15:33Z memes $

(if (memq window-system '(x win32 w32))
    (progn
      ;; Launch planner in new frame, if configured
      (my-run-command-new-frame 'plan)
      ))
