;;; camel.el --- configurable support for CamelCase links in any buffer

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole;;; @@ CamelCase <dto@gnu.org>
;; Keywords: hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program adds support for CamelCase words that can be used as
;; Wiki links in Emacs buffers.

;; For discussion of the issues surrounding CamelCase words, see:
;; http://en.wikipedia.org/wiki/CamelCase
;; and also http://www.c2.com/cgi/wiki/wiki?CamelCase

;; TODO click to open
;; TODO right-click context menu

;;; Code:

(defvar *camel-regexp* "\\<[A-Z][a-z]+[A-Z][a-z]+\\([A-Z][a-z]+\\)*\\>")

(defvar *camel-link-handler* nil
  "Function to handle links.")

(defun camel-install-link-handler (handler)
  (assert (functionp handler))
  (setf *camel-link-handler* handler))

(defun camel-handle-link (word)
  (when (functionp *camel-link-handler*)
    (funcall *camel-link-handler* word)))

(defun camel-follow-link-at-point ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when (and (stringp word)
	       (string-match *camel-regexp* word))
      (camel-handle-link word))))

(defface camel-face
'((t (:foreground "cyan" :underline "yellow")))
  "Face for CamelCase links.")

(defvar camel-face 'camel-face)

(defvar camel-map nil)
(when (null camel-map)
  (setq camel-map (make-sparse-keymap))
  (define-key camel-map (kbd "<f9>" 'camel-mode))

;; optionally in your .emacs:
;; (global-set-key (kbd "<f9>") 'camel-mode)

(define-minor-mode camel-mode
  "Easily toggle configurable support for CamelCase words in any buffer."
  nil 
  :lighter " Camel"
  :keymap camel-map
  (if camel-mode
      (camel-enable)
    (camel-disable)))

(defun camel-enable ()
  (camel-do-font-lock 'font-lock-add-keywords)
  (font-lock-fontify-buffer))

(defun camel-disable ()
  (camel-do-font-lock 'font-lock-remove-keywords)
  ;; (remove-text-properties (point-min) (point-max) '(camel-fontified))
  (font-lock-fontify-buffer))

(defun camel-do-font-lock (add-or-remove)
  (funcall add-or-remove nil
	   `((,*camel-regexp* 0 ,camel-face prepend))))

(provide 'camel)
;;; camel.el ends here




