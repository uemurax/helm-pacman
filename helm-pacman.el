;;; helm-pacman.el - Helm interface for Pacman
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-19 16:49:20 tuemura>
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
  (helm :sources (helm-pacman-sync-group-build-source "Sync group")
        :buffer "*helm-pacman-sync-group*"))

;;;;; Follow group

(defun helm-pacman-sync-group-follow-candidates (groups)
  (split-string (shell-command-to-string (concat "pacman -Sg "
                                                 (mapconcat #'identity groups " ")))
                "\n"))

(defun helm-pacman-sync-group-follow-display-to-real (candidate)
  (cadr (split-string candidate " ")))

(defun helm-pacman-sync-group-follow-build-source (name groups)
  (helm-build-sync-source name
    :candidates (helm-pacman-sync-group-follow-candidates groups)
    :display-to-real 'helm-pacman-sync-group-follow-display-to-real
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

;;;; Put together

;;;###autoload
(defun helm-pacman ()
  "Helm for Pacman."
  (interactive)
  (helm :sources (list (helm-pacman-sync-build-source "Sync")
                       (helm-pacman-sync-group-build-source "Sync - group")
                       (helm-pacman-query-build-source "Query"))
        :buffer "*helm-pacman*"))

(provide 'helm-pacman)

;;; helm-pacman.el ends here.
