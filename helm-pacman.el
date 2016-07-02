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

(eval-when-compile
  (defmacro make-helm-action (action function &optional also-persistent)
    "Define ACTION which runs FUNCTION in `helm-pacman' session.
If ALSO-PERSISTENT is non-nil, define `ACTION-persistent'
which runs FUNCTION in `helm-pacman' session without exitting session."
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
  "Define an action NAME which executes the command `pacman -OPERATION TARGETS...'.
If SUDO is non-nil, it executes the command `sudo pacman -OPERATION TARGETS...'."
  (let* ((command (format "%spacman -%s "
                          (if sudo "sudo " "")
                          operation))
         (doc (format "Run `%sTARGETS...'." command)))
    `(progn
       (defun ,name (&rest _ignore)
         ,doc
         (async-shell-command (concat ,command
                                      (helm-pacman-make-arguments)))))))

;;;; Sync

(defun helm-pacman-sync-candidates-process ()
  "Produce a list of packages."
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
   "Download package(s)" 'helm-pacman-sync-download)
  "Actions for `helm-pacman-sync'.")

(defvar helm-pacman-sync-keymap
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v '(("C-c RET" . helm-pacman-sync-run-install)
                 ("C-c w" . helm-pacman-sync-run-download)))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-pacman-sync'.")

(defun helm-pacman-sync-build-source (name &rest args)
  "Build source for `helm-pacman-sync'."
  (helm-build-async-source name
    :candidates-process 'helm-pacman-sync-candidates-process
    :action 'helm-pacman-sync-actions
    :keymap helm-pacman-sync-keymap))

;;;###autoload
(defun helm-pacman-sync ()
  "Helm for `pacman -S'."
  (interactive)
  (helm :sources (helm-pacman-sync-build-source "Sync")
        :buffer "*helm-pacman-sync*"))

;;;; Sync groups

(defun helm-pacman-sync-group-candidates ()
  "Produce a list of groups."
  (split-string (shell-command-to-string "pacman -Sg")
                "\n"))

(helm-pacman-defaction helm-pacman-sync-group-show "Sg")

(defvar helm-pacman-sync-group-actions
  (helm-make-actions
   "Show group(s)" 'helm-pacman-sync-group-show
   "Follow group(s)" 'helm-pacman-sync-group-follow
   "Install group(s)" 'helm-pacman-sync-install
   "Download group(s)" 'helm-pacman-sync-download)
  "Actions for `helm-pacman-sync-group'.")

(defvar helm-pacman-sync-group-keymap
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v '(("C-c RET" . helm-pacman-sync-group-run-follow)
                 ("C-c i" . helm-pacman-sync-run-install)
                 ("C-c w" . helm-pacman-sync-run-download)))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-pacman-sync-group'.")

(defun helm-pacman-sync-group-build-source (name &rest args)
  "Build source for `helm-pacman-sync-group'."
  (helm-build-sync-source name
    :candidates 'helm-pacman-sync-group-candidates
    :action 'helm-pacman-sync-group-actions
    :keymap helm-pacman-sync-group-keymap))

;;;###autoload
(defun helm-pacman-sync-group ()
  "Helm for `pacman -Sg'."
  (interactive)
  (helm :sources (helm-pacman-sync-group-build-source "Sync - group")
        :buffer "*helm-pacman-sync-group*"))

;;;;; Follow group

(defun helm-pacman-sync-group-follow-candidates (groups)
  "Produce a list of packages in GROUPS."
  (split-string (shell-command-to-string (concat "pacman -Sg "
                                                 (mapconcat #'identity groups " ")))
                "\n"))

(defun helm-pacman-group-display-to-real (candidate)
  "Convert a display form `GROUP PACKAGE' to a real form `PACKAGE'."
  (cadr (split-string candidate " ")))

(defun helm-pacman-sync-group-follow-build-source (name groups)
  "Build source for `helm-pacman-sync-group-follow'."
  (helm-build-sync-source name
    :candidates (helm-pacman-sync-group-follow-candidates groups)
    :display-to-real 'helm-pacman-group-display-to-real
    :action 'helm-pacman-sync-actions
    :keymap helm-pacman-sync-keymap))

(defun helm-pacman-sync-group-follow (_ignore)
  "Spawn a `helm-pacman-sync' session on the selected candidates."
  (helm :sources (helm-pacman-sync-group-follow-build-source "Sync"
                                                             (helm-marked-candidates))
        :buffer "*helm-pacman-sync*"))

(make-helm-action helm-pacman-sync-group-run-follow helm-pacman-sync-group-follow)

;;;; Query

(defun helm-pacman-query-candidates-process ()
  "Produce a list of installed packages."
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
   "Remove package(s)" 'helm-pacman-remove)
  "Actions for `helm-pacman-query'.")

(defvar helm-pacman-query-keymap
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v '(("M-L" . helm-pacman-query-run-list)
                 ("C-c l" . helm-pacman-query-run-list-persistent)
                 ("M-U" . helm-pacman-run-upgrade)
                 ("C-c r" . helm-pacman-run-remove)))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-pacman-query'.")

(defun helm-pacman-query-build-source (name &rest args)
  "Build source for `helm-pacman-query'."
  (helm-build-async-source name
    :candidates-process 'helm-pacman-query-candidates-process
    :action 'helm-pacman-query-actions
    :keymap helm-pacman-query-keymap))

;;;###autoload
(defun helm-pacman-query ()
  "Helm for `pacman -Q'."
  (interactive)
  (helm :sources (helm-pacman-query-build-source "Query")
        :buffer "*helm-pacman-query*"))

;;;; Query groups

(defun helm-pacman-query-group-candidates ()
  "Produce a list of installed groups."
  (split-string (shell-command-to-string "pacman -Qg")
                "\n"))

(defun helm-pacman-query-group-build-source (name &rest args)
  "Build source for `helm-pacman-query-group'."
  (helm-build-sync-source name
    :candidates 'helm-pacman-query-group-candidates
    :display-to-real 'helm-pacman-group-display-to-real
    :action 'helm-pacman-query-actions
    :keymap helm-pacman-query-keymap))

;;;###autoload
(defun helm-pacman-query-group ()
  "Helm for `pacman -Qg'."
  (interactive)
  (helm :sources (helm-pacman-query-group-build-source "Query - group")
        :buffer "*helm-pacman-query-group*"))

;;;; AUR

(defvar helm-pacman-aur-host "https://aur.archlinux.org")
(defvar helm-pacman-aur-rpc "/rpc/")
(defvar helm-pacman-aur-version 5)

(defun helm-pacman-aur-rpc-uri (query)
  "Build a uri for aurweb RPC Interface."
  (concat helm-pacman-aur-host
          helm-pacman-aur-rpc
          (format "?v=%d&" helm-pacman-aur-version)
          (url-build-query-string query)))

(defclass helm-pacman-source-aur (helm-source-async)
  ((candidates-process :initform #'helm-pacman-aur-candidates-process)))

(defun helm-pacman-aur-candidates-process ()
  (let ((proc (start-process "helm-pacman-aur" nil "cat")))
    (url-retrieve (concat (helm-pacman-aur-rpc-uri
                             `((type "search")))
                          "&arg="
                          (replace-regexp-in-string "\\s-+" "+" helm-pattern))
                  'helm-pacman-aur-candidates-callback
                  (list proc))
    proc))

(defun helm-pacman-aur-candidates-callback-error (process obj))

(defun helm-pacman-aur-candidates-callback-search (process obj)
  (mapc (lambda (x)
          (process-send-string process
                               (concat (cdr (assq 'Name x))
                                       "\n")))
        (cdr (assq 'results obj))))

(defun helm-pacman-aur-candidates-callback (status process &rest _ignore)
  (when (process-live-p process)
    (goto-char (point-min))
    (search-forward "\n\n")
    (let* ((obj (json-read))
           (type (cdr (assq 'type obj))))
      (cond ((equal type "error")
             (helm-pacman-aur-candidates-callback-error process obj))
            ((equal type "search")
             (helm-pacman-aur-candidates-callback-search process obj))
            (t
             (message "Wrong type: %S" type)))
      (process-send-eof process))))

(defun helm-pacman-aur-get-info (pkgs callback &optional cbargs)
  (when pkgs
    (url-retrieve (helm-pacman-aur-rpc-uri
                   `((type "info")
                     ("arg[]" ,@pkgs)))
                  (lambda (status callback &rest cbargs)
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (let* ((obj (json-read))
                           (type (cdr (assq 'type obj))))
                      (apply callback
                             (cond ((equal type "multiinfo")
                                    (cdr (assq 'results obj)))
                                   (t (message "Wrong type: %S" type)
                                      nil))
                             cbargs)))
                  (apply #'list callback cbargs))))

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
  "Show package(s) infomation."
  (let ((buf (get-buffer-create "*helm-pacman-aur-info*")))
    (switch-to-buffer "*helm-pacman-aur-info*")
    (helm-pacman-aur-get-info (helm-marked-candidates)
                              (lambda (obj buf)
                                (with-current-buffer buf
                                  (erase-buffer)
                                  (mapc (lambda (p)
                                          (insert (helm-pacman-aur-format-package p) "\n\n"))
                                        obj)))
                              (list buf))))

(defun helm-pacman-aur-get-callback (status dir &rest _ignore)
  (goto-char (point-min))
  (search-forward "\n\n")
  (shell-command-on-region (point) (point-max)
                           (format "tar xz -C '%s'" dir)))

(defun helm-pacman-aur-get (_ignore)
  "Get package(s)' PKGBUILD."
  (let ((pkgs (helm-marked-candidates)))
    (let ((dir (expand-file-name (read-directory-name "Download into: " "~/.local/source/"))))
      (unless (file-exists-p dir)
        (make-directory dir t))
      (helm-pacman-aur-get-info
       pkgs
       (lambda (obj dir)
         (mapc `(lambda (v)
                  (let ((path (cdr (assq 'URLPath v))))
                    (message "Download %s" path)
                    (url-retrieve (concat helm-pacman-aur-host
                                          path)
                                  'helm-pacman-aur-get-callback
                                  (list ,dir))))
               obj))
       (list dir)))))
(make-helm-action helm-pacman-aur-run-get helm-pacman-aur-get)

(defcustom helm-pacman-aur-helper-command "yaourt -S"
  "Command to install AUR packages."
  :group 'helm-pacman
  :type 'string)

(defun helm-pacman-aur-install (&rest _ignore)
  "Run `yaourt -S TARGET...'"
  (async-shell-command (concat helm-pacman-aur-helper-command
                               " "
                               (helm-pacman-make-arguments))))
(make-helm-action helm-pacman-aur-run-install helm-pacman-aur-install)

(defvar helm-pacman-aur-actions
  (helm-make-actions
   "Show package(s)" 'helm-pacman-aur-info
   "Get package(s) PKGBUILD" 'helm-pacman-aur-get
   "Install package(s)" 'helm-pacman-aur-install)
  "Actions for `helm-pacman-aur'.")

(defvar helm-pacman-aur-keymap
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v '(("C-c w" . helm-pacman-aur-run-get)
                 ("C-c i" . helm-pacman-aur-run-install)))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-pacman-aur'.")

(defun helm-pacman-aur-build-source (name &rest args)
  "Build source for `helm-pacman-aur'."
  (helm-make-source name 'helm-pacman-source-aur
    :action 'helm-pacman-aur-actions
    :keymap helm-pacman-aur-keymap))

;;;###autoload
(defun helm-pacman-aur ()
  "Helm for AUR."
  (interactive)
  (helm :sources (helm-pacman-aur-build-source "AUR")
        :buffer "*helm-pacman-aur*"))

;;;; Put together

;;;###autoload
(defun helm-pacman ()
  "Helm for Pacman.

It is a mixture of `helm-pacman-sync', `helm-pacman-sync-group',
`helm-pacman-query', `helm-pacman-query-group' and `helm-pacman-aur'."
  (interactive)
  (helm :sources (list (helm-pacman-sync-build-source "Sync")
                       (helm-pacman-sync-group-build-source "Sync - group")
                       (helm-pacman-query-build-source "Query")
                       (helm-pacman-query-group-build-source "Query - group")
                       (helm-pacman-aur-build-source "AUR"))
        :buffer "*helm-pacman*"))

(provide 'helm-pacman)

;;; helm-pacman.el ends here.
