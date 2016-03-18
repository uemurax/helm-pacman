;;; helm-pacman.el - Helm interface for Pacman
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-19 00:44:24 tuemura>
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

(defun helm-pacman-sync-candidates ()
  (split-string (shell-command-to-string "pacman -Slq")
                "\n"))

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
  (helm-build-sync-source name
    :candidates (helm-pacman-sync-candidates)
    :action 'helm-pacman-sync-actions
    :keymap helm-pacman-sync-keymap))

;;;###autoload
(defun helm-pacman-sync ()
  (interactive)
  (helm :sources (helm-pacman-sync-build-source "Sync")
        :buffer "*helm-pacman-sync*"))

;;;; Query

(defun helm-pacman-query-candidates ()
  (split-string (shell-command-to-string "pacman -Qq")
                "\n"))

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
  (helm-build-sync-source name
    :candidates (helm-pacman-query-candidates)
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
                       (helm-pacman-query-build-source "Query"))
        :buffer "*helm-pacman*"))

(provide 'helm-pacman)

;;; helm-pacman.el ends here.
