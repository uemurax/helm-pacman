;;; helm-pacman.el - Helm interface for Pacman
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-18 23:45:04 tuemura>
;;
;;; Code:

(require 'helm)

(eval-when-compile
  (defmacro make-helm-action (action function)
    (let ((action-doc (format "Run `%s' in `helm-pacman' session." function)))
      `(progn
         (defun ,action ()
           (interactive)
           (with-helm-alive-p
             (helm-exit-and-execute-action ',function)))
         (put ',action 'helm-only t)))))

(defun helm-pacman-sync ()
  (split-string (shell-command-to-string "pacman -Slq")
                "\n"))

(defun helm-pacman-sync-info (_ignore)
  (switch-to-buffer "*helm-pacman-sync-info*")
  (erase-buffer)
  (shell-command (concat "pacman -Si "
                         (mapconcat 'identity (helm-marked-candidates) " "))
                 t))

(defun helm-pacman-sync-install (_ignore)
  (async-shell-command (concat "sudo pacman -S "
                         (mapconcat 'identity (helm-marked-candidates) " "))))

(make-helm-action helm-pacman-sync-run-install helm-pacman-sync-install)

(defun helm-pacman-sync-download (_ignore)
  (async-shell-command (concat "sudo pacman -Sw "
                               (mapconcat 'identity (helm-marked-candidates) " "))))

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

(defun helm-pacman-build-sync-source (name &rest args)
  (helm-build-sync-source name
    :candidates (helm-pacman-sync)
    :action 'helm-pacman-sync-actions
    :keymap helm-pacman-sync-keymap))

;;;###autoload
(defun helm-pacman ()
  "Helm for Pacman."
  (interactive)
  (helm :sources (list (helm-pacman-build-sync-source "Sync"))
        :buffer "*helm-pacman*"))

(provide 'helm-pacman)

;;; helm-pacman.el ends here.
