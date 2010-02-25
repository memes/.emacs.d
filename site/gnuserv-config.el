;; Configuration options for gnuserv
;; $Id: gnuserv-config.el 2 2007-10-18 23:31:31Z memes $


;; Only setup gnuserv if in a windowed environment
(if window-system
    (progn
      (autoload 'gnuserv-start "gnuserv-compat"
	"Allow Emacs to accept client connections" t)
      (gnuserv-start)
      )
  )