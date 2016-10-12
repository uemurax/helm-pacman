;;; helm-pacman.el --- Helm interface for Pacman
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Version: 0.1
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Package-Requires: ((helm-core "1.9.7"))
;;
;;; Code:

(require 'helm)

(defgroup helm-pacman ()
  "Helm for pacman"
  :group 'helm)

(defcustom helm-pacman-backend-alist
  '((pacman (command "pacman" "--color" "never")
            (sudo . t)
            (sync-search "-Ssq")
            (sync-info "-Si")
            (sync-install "-S")
            (sync-download "-Sw")
            (query-search "-Qsq")
            (query-info "-Qi")
            (query-list "-Ql")
            (remove "-Rs"))
    (yaourt (command "yaourt" "--nocolor")
            (sync-search "-Ssq")
            (sync-info "-Si")
            (sync-install "-S")
            (sync-download "-Sw")))
  "Alist of `(NAME SPECS..)'.

Each SPEC is an alist of `(KEY ARGS..)'."
  :group 'helm-pacman
  :type 'list)

(defcustom helm-pacman-backend
  (cond ((executable-find "yaourt") 'yaourt)
        (t 'pacman))
  "Pacman backend. One of keys in `helm-pacman-backend-alist'."
  :group 'helm-pacman
  :type 'symbol)

(defun helm-pacman-get-backend ()
  (cdr (assq helm-pacman-backend helm-pacman-backend-alist)))

(defun helm-pacman-default-backend ()
  (cdr (assq 'pacman helm-pacman-backend-alist)))

(defun helm-pacman-build-command (backend key args &optional sudo)
  (let ((backend (if (assq key backend)
                     backend
                   (helm-pacman-default-backend))))
    `(,@(when (and sudo
                   (assq 'sudo backend))
          '("sudo"))
      ,@(cdr (assq 'command backend))
      ,@(cdr (assq key backend))
      ,@args)))

(defun helm-pacman-build-shell-command (backend key args &optional sudo)
  (mapconcat #'shell-quote-argument
             (helm-pacman-build-command backend key args sudo)
             " "))

(defcustom helm-pacman-output-buffer "*helm-pacman-output*"
  "Output buffer name."
  :group 'helm-pacman
  :type 'string)

(defun helm-pacman-action (key &optional sudo)
  (async-shell-command (helm-pacman-build-shell-command
                        (helm-pacman-get-backend)
                        key
                        (helm-marked-candidates)
                        sudo)
                       helm-pacman-output-buffer))

;;;; Sync

(defun helm-pacman-sync-candidates-process ()
  "Produce a list of packages."
  (apply #'start-process "pacman-sync" nil
         (helm-pacman-build-command
          (helm-pacman-get-backend)
          'sync-search
          (split-string helm-pattern " " t))))

(defun helm-pacman-sync-info (&rest _ignore)
  (helm-pacman-action 'sync-info))

(defun helm-pacman-sync-install (&rest _ignore)
  (helm-pacman-action 'sync-install t))

(defun helm-pacman-sync-download (&rest _ignore)
  (helm-pacman-action 'sync-download t))

(defvar helm-pacman-sync-actions
  (helm-make-actions
   "Show package(s)" 'helm-pacman-sync-info
   "Install package(s)" 'helm-pacman-sync-install
   "Download package(s)" 'helm-pacman-sync-download)
  "Actions for `helm-pacman-sync'.")

(defun helm-pacman-sync-build-source (name &rest args)
  "Build source for `helm-pacman-sync'."
  (apply #'helm-make-source name 'helm-source-async
         :candidates-process 'helm-pacman-sync-candidates-process
         :action 'helm-pacman-sync-actions
         args))

(defvar helm-source-pacman-sync
  (helm-pacman-sync-build-source "Sync"))

;;;###autoload
(defun helm-pacman-sync ()
  "Helm for `pacman -S'."
  (interactive)
  (helm :sources '(helm-source-pacman-sync)
        :buffer "*helm-pacman-sync*"))

;;;; Query

(defun helm-pacman-query-candidates-process ()
  "Produce a list of installed packages."
  (apply #'start-process "pacman-query" nil
         (helm-pacman-build-command
          (helm-pacman-get-backend)
          'query-search
          (split-string helm-pattern " " t))))

(defun helm-pacman-query-info (&rest _ignore)
  (helm-pacman-action 'query-info))

(defun helm-pacman-query-list (&rest _ignore)
  (helm-pacman-action 'query-list))

(defun helm-pacman-remove (&rest _ignore)
  (helm-pacman-action 'remove t))

(defvar helm-pacman-query-actions
  (helm-make-actions
   "Show package(s)" 'helm-pacman-query-info
   "List package(s)' files" 'helm-pacman-query-list
   "Remove package(s)" 'helm-pacman-remove)
  "Actions for `helm-pacman-query'.")

(defun helm-pacman-query-build-source (name &rest args)
  "Build source for `helm-pacman-query'."
  (apply #'helm-make-source name 'helm-source-async
         :candidates-process 'helm-pacman-query-candidates-process
         :action 'helm-pacman-query-actions
         args))

(defvar helm-source-pacman-query (helm-pacman-query-build-source "Query"))

;;;###autoload
(defun helm-pacman-query ()
  "Helm for `pacman -Q'."
  (interactive)
  (helm :sources '(helm-source-pacman-query)
        :buffer "*helm-pacman-query*"))

;;;; Put together

;;;###autoload
(defun helm-pacman ()
  "Helm for Pacman."
  (interactive)
  (helm :sources '(helm-source-pacman-sync helm-source-pacman-query)
        :buffer "*helm-pacman*"))

(provide 'helm-pacman)

;;; helm-pacman.el ends here.
