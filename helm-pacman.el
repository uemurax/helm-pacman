;;; helm-pacman.el - Helm interface for Pacman
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-19 22:20:52 tuemura>
;;
;;; Code:

(require 'helm)

(eval-when-compile
  (defmacro make-helm-action (action function &optional also-persistent)
    (let ((action-doc (format "Run `%s' in `helm-pacman' session." function))
          (persistent (intern (format "%s-persistent" action)))
          (persistent-doc (format "Run `%s' in `helm-pacman' session without exitting session."
                                  function)))
      `(progn
         (defun ,action ()
           ,action-doc
           (interactive)
           (with-helm-alive-p
             (helm-exit-and-execute-action ',function)))
         (put ',action 'helm-only t)
         ,@(when also-persistent
             `((defun ,persistent ()
                 ,persistent-doc
                 (interactive)
                 (with-helm-alive-p
                   (helm-attrset 'pacman-persistent-action '(,function . never-split))
                   (helm-execute-persistent-action 'pacman-persistent-action)))
               (put ',persistent 'helm-only t)))))))

(defun helm-pacman-make-arguments ()
  "Make space-separated arguments from `helm-marked-candidates'."
  (mapconcat 'identity (helm-marked-candidates) " "))

(defmacro helm-pacman-defaction (name operation &optional sudo)
  (let ((command (format "%spacman -%s "
                         (if sudo "sudo " "")
                         operation)))
    `(progn
       (defun ,name (&rest _ignore)
         (async-shell-command (concat ,command
                                      (helm-pacman-make-arguments)))))))

;;;; Sync

(defun helm-pacman-sync-candidates-process ()
  (apply #'start-process "pacman-sync" nil "pacman"
         "-Ssq"
         (split-string helm-pattern " ")))

(helm-pacman-defaction helm-pacman-sync-info "Si")

(helm-pacman-defaction helm-pacman-sync-install "S" t)
(make-helm-action helm-pacman-sync-run-install helm-pacman-sync-install)

(helm-pacman-defaction helm-pacman-sync-download "Sw" t)
(make-helm-action helm-pacman-sync-run-download helm-pacman-sync-download)

(defvar helm-pacman-sync-actions
  (helm-make-actions
   "Show package(s)" 'helm-pacman-sync-info
   "Install package(s)" 'helm-pacman-sync-install
   "Download package(s)" 'helm-pacman-sync-download))

(defvar helm-pacman-sync-keymap
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v '(("C-c RET" . helm-pacman-sync-run-install)
                 ("C-c w" . helm-pacman-sync-run-download)))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-pacman-sync-build-source (name &rest args)
  (helm-build-async-source name
    :candidates-process 'helm-pacman-sync-candidates-process
    :action 'helm-pacman-sync-actions
    :keymap helm-pacman-sync-keymap))

;;;###autoload
(defun helm-pacman-sync ()
  (interactive)
  (helm :sources (helm-pacman-sync-build-source "Sync")
        :buffer "*helm-pacman-sync*"))

;;;; Sync groups

(defun helm-pacman-sync-group-candidates ()
  (split-string (shell-command-to-string "pacman -Sg")
                "\n"))

(helm-pacman-defaction helm-pacman-sync-group-show "Sg")

(defvar helm-pacman-sync-group-actions
  (helm-make-actions
   "Show group(s)" 'helm-pacman-sync-group-show
   "Follow group(s)" 'helm-pacman-sync-group-follow
   "Install group(s)" 'helm-pacman-sync-install
   "Download group(s)" 'helm-pacman-sync-download))

(defvar helm-pacman-sync-group-keymap
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v '(("C-c RET" . helm-pacman-sync-group-run-follow)
                 ("C-c i" . helm-pacman-sync-run-install)
                 ("C-c w" . helm-pacman-sync-run-download)))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-pacman-sync-group-build-source (name &rest args)
  (helm-build-sync-source name
    :candidates 'helm-pacman-sync-group-candidates
    :action 'helm-pacman-sync-group-actions
    :keymap helm-pacman-sync-group-keymap))

;;;###autoload
(defun helm-pacman-sync-group ()
  (interactive)
  (helm :sources (helm-pacman-sync-group-build-source "Sync - group")
        :buffer "*helm-pacman-sync-group*"))

;;;;; Follow group

(defun helm-pacman-sync-group-follow-candidates (groups)
  (split-string (shell-command-to-string (concat "pacman -Sg "
                                                 (mapconcat #'identity groups " ")))
                "\n"))

(defun helm-pacman-group-display-to-real (candidate)
  (cadr (split-string candidate " ")))

(defun helm-pacman-sync-group-follow-build-source (name groups)
  (helm-build-sync-source name
    :candidates (helm-pacman-sync-group-follow-candidates groups)
    :display-to-real 'helm-pacman-group-display-to-real
    :action 'helm-pacman-sync-actions
    :keymap helm-pacman-sync-keymap))

(defun helm-pacman-sync-group-follow (_ignore)
  (helm :sources (helm-pacman-sync-group-follow-build-source "Sync"
                                                             (helm-marked-candidates))
        :buffer "*helm-pacman-sync*"))

(make-helm-action helm-pacman-sync-group-run-follow helm-pacman-sync-group-follow)

;;;; Query

(defun helm-pacman-query-candidates-process ()
  (apply #'start-process "pacman-query" nil "pacman"
                 "-Qsq"
                 (split-string helm-pattern " ")))

(helm-pacman-defaction helm-pacman-query-info "Qi")

(helm-pacman-defaction helm-pacman-query-list "Ql")
(make-helm-action helm-pacman-query-run-list helm-pacman-query-list t)

(helm-pacman-defaction helm-pacman-remove "R" t)
(make-helm-action helm-pacman-run-remove helm-pacman-remove)

(helm-pacman-defaction helm-pacman-upgrade "Syu" t)
(make-helm-action helm-pacman-run-upgrade helm-pacman-upgrade)

(defvar helm-pacman-query-actions
  (helm-make-actions
   "Show package(s)" 'helm-pacman-query-info
   "List package(s)' files" 'helm-pacman-query-list
   "Upgrade package(s)" 'helm-pacman-upgrade
   "Remove package(s)" 'helm-pacman-remove))

(defvar helm-pacman-query-keymap
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v '(("M-L" . helm-pacman-query-run-list)
                 ("C-c l" . helm-pacman-query-run-list-persistent)
                 ("M-U" . helm-pacman-run-upgrade)
                 ("C-c r" . helm-pacman-run-remove)))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-pacman-query-build-source (name &rest args)
  (helm-build-async-source name
    :candidates-process 'helm-pacman-query-candidates-process
    :action 'helm-pacman-query-actions
    :keymap helm-pacman-query-keymap))

;;;###autoload
(defun helm-pacman-query ()
  (interactive)
  (helm :sources (helm-pacman-query-build-source "Query")
        :buffer "*helm-pacman-query*"))

;;;; Query groups

(defun helm-pacman-query-group-candidates ()
  (split-string (shell-command-to-string "pacman -Qg")
                "\n"))

(defun helm-pacman-query-group-build-source (name &rest args)
  (helm-build-sync-source name
    :candidates 'helm-pacman-query-group-candidates
    :display-to-real 'helm-pacman-group-display-to-real
    :action 'helm-pacman-query-actions
    :keymap helm-pacman-query-keymap))

;;;###autoload
(defun helm-pacman-query-group ()
  (interactive)
  (helm :sources (helm-pacman-query-group-build-source "Query - group")
        :buffer "*helm-pacman-query-group*"))

;;;; AUR

(defvar helm-pacman-aur-host "https://aur.archlinux.org")
(defvar helm-pacman-aur-rpc "/rpc/")
(defvar helm-pacman-aur-version 5)

(defun helm-pacman-aur-rpc-uri (query)
  (concat helm-pacman-aur-host
          helm-pacman-aur-rpc
          (format "?v=%d&" helm-pacman-aur-version)
          (url-build-query-string query)))

(defclass helm-pacman-aur-source (helm-source-in-buffer)
  ((candidates-cache :initform t)
   (pattern-cache :initform "")))

(defun helm-pacman-aur-candidates-callback-error (source obj)
  (message (cdr (assq 'error obj)))
  (setcdr (assq 'candidates-cache source) nil))

(defun helm-pacman-aur-candidates-callback-search (source obj)
  (setcdr (assq 'candidates-cache source)
          (mapcar (lambda (x) (cons (cdr (assq 'Name x)) x))
                  (cdr (assq 'results obj)))))

(defvar helm-pacman-aur-candidates-callback-count 0)

(defun helm-pacman-aur-candidates-callback (status source count &rest _ignore)
  (when (= count helm-pacman-aur-candidates-callback-count)
    (goto-char (point-min))
    (search-forward "\n\n")
    (let* ((obj (json-read))
           (type (cdr (assq 'type obj))))
      (cond ((equal type "error")
             (helm-pacman-aur-candidates-callback-error source obj))
            ((equal type "search")
             (helm-pacman-aur-candidates-callback-search source obj))
            (t
             (message "Wrong type: %S" type)))
      (helm-update))))

(defun helm-pacman-aur-candidates (&optional source)
  (let ((src (or source (helm-get-current-source))))
    (unless (equal (cdr (assq 'pattern-cache src)) helm-pattern)
      (setcdr (assq 'pattern-cache src) helm-pattern)
      (message "retrieving AUR packages...")
      (setq helm-pacman-aur-candidates-callback-count
            (1+ helm-pacman-aur-candidates-callback-count))
      (url-retrieve (concat (helm-pacman-aur-rpc-uri
                             `((type "search")))
                            "&arg="
                            (replace-regexp-in-string "\\s-+" "+" helm-pattern))
                    'helm-pacman-aur-candidates-callback
                    (list src
                          helm-pacman-aur-candidates-callback-count)))
    (cdr (assq 'candidates-cache src))))

(defun helm-pacman-aur-format-package (pkg)
  (mapconcat (lambda (v)
               (format "%-20s : %s"
                       (car v)
                       (let ((x (cdr v)))
                         (cond ((and (not (stringp x)) (sequencep x))
                                (mapconcat (lambda (y) (format "%s" y))
                                           x
                                           (format "\n%-20s   " "")))
                               (t x)))))
             pkg
             "\n"))

(defun helm-pacman-aur-info (_ignore)
  (switch-to-buffer "*helm-pacman-aur-info*")
  (erase-buffer)
  (mapc (lambda (p) (insert (helm-pacman-aur-format-package p) "\n\n"))
        (with-current-buffer
            (url-retrieve-synchronously (helm-pacman-aur-rpc-uri
                                         `((type "info")
                                           ("arg[]" ,@(mapcar (lambda (x) (cdr (assq 'Name x)))
                                                              (helm-marked-candidates))))))
          (goto-char (point-min))
          (search-forward "\n\n")
          (let* ((obj (json-read))
                 (type (cdr (assq 'type obj))))
            (cond ((equal type "multiinfo")
                   (cdr (assq 'results obj)))
                  (t (message "Wrong type: %S" type)
                     nil))))))

(defvar helm-pacman-aur-actions
  (helm-make-actions
   "Show package(s)" 'helm-pacman-aur-info))

(defun helm-pacman-aur-build-source (name &rest args)
  (helm-make-source name 'helm-pacman-aur-source
    :candidates 'helm-pacman-aur-candidates
    :action 'helm-pacman-aur-actions
    :delayed 0.2))

;;;###autoload
(defun helm-pacman-aur ()
  (interactive)
  (helm :sources (helm-pacman-aur-build-source "AUR")
        :buffer "*helm-pacman-aur*"))

;;;; Put together

;;;###autoload
(defun helm-pacman ()
  "Helm for Pacman."
  (interactive)
  (helm :sources (list (helm-pacman-sync-build-source "Sync")
                       (helm-pacman-sync-group-build-source "Sync - group")
                       (helm-pacman-query-build-source "Query")
                       (helm-pacman-query-group-build-source "Query - group")
                       (helm-pacman-aur-build-source "AUR"))
        :buffer "*helm-pacman*"))

(provide 'helm-pacman)

;;; helm-pacman.el ends here.
