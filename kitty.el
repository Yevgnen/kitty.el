;;; kitty.el --- Kitty in Emacs. -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
;; Keywords:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;
;; See documentation on https://github.com/Yevgnen/kitty.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom kitty-socket "unix:/tmp/kitty"
  "Default socket for Kitty.")

(defcustom kitty-python-breakpoint-statement "breakpoint()"
  "Breakpoint statement for Python.")

(defface kitty-breakpoint-face
  '((t (:inherit hl-line :extend nil)))
  "Face for highlighting breakpoints.")

(defun kitty--focus-p (x)
  (not (eq (gethash "is_focused" x) :false)))

(defun kitty--filter-map (predicate seq)
  (seq-filter (lambda (x)
                (not (null x)))
              (seq-map predicate seq)))

(defun kitty--buffer-name (default &optional buffer-name)
  (or buffer-name default))

;;;###autoload
(defun kitty-run (cmd &optional buffer &rest args)
  (apply #'call-process
         (append `("kitty" nil ,(not (null buffer)) nil "@" "--to" ,kitty-socket)
                 `(,cmd)
                 args)))

;;;###autoload
(defun kitty-focus (&optional tab-args window-args)
  (interactive)
  (do-applescript
   (mapconcat #'identity
              '("tell application \"kitty\""
                "    activate"
                "end tell")
              "\n"))
  (if tab-args
      (apply #'kitty-focus-tab (list nil tab-args)))
  (if window-args
      (apply #'kitty-focus-window (list nil window-args))))

;;;###autoload
(defun kitty-focus-tab (&optional buffer args)
  (apply #'kitty-run
         (append `("focus-tab" ,buffer)
                 args)))

;;;###autoload
(defun kitty-focus-window (&optional buffer args)
  (apply #'kitty-run
         (append `("focus-window" ,buffer)
                 args)))

;;;###autoload
(defun kitty-launch (&optional buffer &rest args)
  (apply #'kitty-run
         (append `("launch" ,buffer)
                 args)))

;;;###autoload
(defun kitty-ls (&optional buffer &rest args)
  (apply #'kitty-run
         (append `("ls" ,buffer)
                 args)))

;;;###autoload
(defun kitty-send-text (&optional buffer &rest args)
  (apply #'kitty-run
         (append `("send-text" ,buffer)
                 args)))

;;;###autoload
(defun kitty-filter-window (&optional instance-filter tab-filter window-filter)
  (with-temp-buffer
    (kitty-ls 'buffer)
    (goto-char (point-min))
    (if-let* ((data (json-parse-buffer))
              (instances (if instance-filter
                             (seq-filter instance-filter data)
                           data))
              (instances (kitty--filter-map
                          (lambda (instance)
                            (when-let* ((tabs (gethash "tabs" instance))
                                        (tabs (if tab-filter
                                                  (seq-filter tab-filter tabs)
                                                tabs))
                                        (tabs (kitty--filter-map
                                               (lambda (tab)
                                                 (when-let* ((windows (gethash "windows" tab))
                                                             (windows (if window-filter
                                                                          (seq-filter window-filter windows)
                                                                        windows)))
                                                   (puthash "windows" windows tab)
                                                   tab))
                                               tabs)))
                              (puthash "tabs" tabs instance)
                              instance))
                          instances))
              (instance (seq-elt instances 0))
              (tabs (gethash "tabs" instance))
              (tab (seq-elt tabs 0))
              (windows (gethash "windows" tab))
              (window (seq-elt windows 0)))
        (list (gethash "id" instance)
              (gethash "id" tab)
              (gethash "id" window)))))

;;;###autoload
(defun kitty-filter-window-2 (&optional instance-filter tab-filter window-filter)
  (with-temp-buffer
    (kitty-ls 'buffer)
    (goto-char (point-min))
    (if-let* ((data (json-parse-buffer))
              (filter-instances (if instance-filter
                                    (seq-filter instance-filter data)
                                  data))
              (filter-instance (car filter-instances))
              (tabs (gethash "tabs" filter-instance))
              (filter-tabs (if tab-filter
                               (seq-filter tab-filter tabs)
                             tabs))
              (filter-tab (car filter-tabs))
              (windows (gethash "windows" filter-tab))
              (filter-windows (if window-filter
                                  (seq-filter window-filter windows)
                                windows))
              (filter-window (car filter-windows)))
        (list (gethash "id" filter-instance)
              (gethash "id" filter-tab)
              (gethash "id" filter-window)))))

;;;###autoload
(defun kitty-get-focus-window ()
  (kitty-filter-window #'kitty--focus-p
                       #'kitty--focus-p
                       #'kitty--focus-p))

;;; Shell

;;;###autoload
(defun kitty-run-bash (&optional bash buffer-name)
  (interactive)
  (let* ((buffer-name (kitty--buffer-name "*Shell*" buffer-name))
         (ids (kitty-filter-window nil
                                   (lambda (x)
                                     (string= (gethash "title" x)
                                              buffer-name)))))
    (if (or current-prefix-arg
            (not ids))
        (progn
          (funcall #'kitty-launch nil
                   "--type" "tab"
                   "--title" buffer-name
                   "--cwd" (or default-directory "~")
                   (or bash (executable-find "bash")))
          (kitty-focus))
      (cl-destructuring-bind (instance-id tab-id window-id)
          ids
        (kitty-focus (list "--match" (format "id:%s" tab-id))
                     (list "--match" (format "id:%s" window-id)))))))

;;; Python

;;;###autoload
(defun kitty-toggle-python-breakpoint (keep-breakpoint &optional dont-save)
  (interactive "P")
  (let ((trace kitty-python-breakpoint-statement))
    (let ((line (thing-at-point 'line)))
      (unless (and line
                   (string-match trace line))
        (previous-line)))
    (let ((line (thing-at-point 'line)))
      (if (and line
               (string-match trace line))
          (unless keep-breakpoint
            (kill-whole-line))
        (progn
          (next-line)
          (back-to-indentation)
          (insert trace)
          (insert "\n")
          (python-indent-line)
          (highlight-lines-matching-regexp trace 'kitty-breakpoint-face)))
      (unless dont-save
        (save-buffer)))))

;;;###autoload
(defun kitty-pdb-debug-file (&optional file-name interpreter debugger run)
  (interactive)
  (when current-prefix-arg
    (if (not file-name)
        (setq file-name (expand-file-name
                         (read-file-name "File: " nil (buffer-file-name) t))))
    (if (not interpreter)
        (setq interpreter (completing-read "Interpreter: "
                                           '("python" "python3" "ipython" "ipython3")
                                           nil t "python")))
    (if (not debugger)
        (setq debugger (completing-read "Debugger: "
                                        '("pdb" "ipdb")
                                        nil t "pdb"))))
  (let ((args (append
               (list "--no-response"
                     "--type"
                     "tab"
                     (executable-find (or interpreter "python"))
                     "-m"
                     (or debugger "pdb"))
               (if run (list "-c" "continue"))
               (list (or file-name (buffer-file-name))))))
    (apply #'kitty-launch
           (append `(nil) args)))
  (kitty-focus))

;;;###autoload
(defun kitty-pdb-debug-file-current-line (&optional file-name interpreter debugger)
  (interactive)
  (kitty-toggle-python-breakpoint t)
  (kitty-pdb-debug-file file-name interpreter debugger 'run)
  (kitty-focus))

;;;###autoload
(defun kitty-run-python (&optional interpreter buffer-name focus)
  (interactive)
  (let ((interpreter (or interpreter (executable-find "ipython")))
        (buffer-name (or buffer-name (kitty--buffer-name "*Python*"))))
    (kitty-launch nil
                  "--type" "tab"
                  "--tab-title" buffer-name
                  interpreter
                  "--TerminalInteractiveShell.autoindent=False"))
  (if focus
      (kitty-focus)))

;;;###autoload
(defun kitty--python-send-string (string &optional buffer &rest args)
  (if (string-match ".\n+." string)
      (let ((file-name (python-shell--save-temp-file (replace-regexp-in-string "\n\\{2,\\}" "\n" string))))
        (apply #'kitty-send-text
               (append
                `(,buffer)
                args
                `("--from-file" ,file-name)))
        (kitty--python-send-string "\n\n" nil))
    (apply #'kitty-send-text
           (append
            `(,buffer)
            args
            `(,string)))))

;;;###autoload
(defun kitty-python-send-string (string &optional interpreter buffer-name focus)
  (let* ((buffer-name (kitty--buffer-name "*Python*" buffer-name))
         (ids (kitty-filter-window nil
                                   (lambda (x)
                                     (string= (gethash "title" x)
                                              buffer-name)))))
    (if ids
        (cl-destructuring-bind (instance-id tab-id window-id)
            ids
          (kitty--python-send-string string nil
                                     "--match-tab" (format "id:%s" tab-id)
                                     "--match" (format "id:%s" window-id)))
      (kitty-run-python interpreter buffer-name focus)
      (kitty--python-send-string string nil
                                 "--match-tab" (format "title:%s" (regexp-quote buffer-name))))
    (if focus
        (kitty-focus))))

;;;###autoload
(defun kitty-python-send-region (begin end &optional interpreter buffer-name focus)
  (interactive "r")
  (kitty-python-send-string (buffer-substring-no-properties begin end) interpreter buffer-name focus))

;;;###autoload
(defun kitty-python-send-buffer (&optional interpreter buffer-name focus)
  (interactive)
  (kitty-python-send-region (point-min) (point-max)))

(provide 'kitty)

;;; kitty.el ends here
