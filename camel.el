;; camel.el --- configurable support for clickable links in any buffer

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

;; This program adds support for clickable CamelCase words that can be
;; used as Wiki links in Emacs buffers.

;; For discussion of the issues surrounding CamelCase words, see:
;; http://en.wikipedia.org/wiki/CamelCase
;; and also http://www.c2.com/cgi/wiki/wiki?CamelCase

;;; Code:

(defvar *camel-regexp* "\\<[A-Z][a-z]+[A-Z][a-z]+\\([A-Z][a-z]+\\)*\\>")

(defvar *camel-link-handler* nil
  "Function to handle links.")

(defun camel-set-link-handler (handler)
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

(defun camel-mouse-follow-link (event)
  (interactive "e")
  (let ((pos (posn-point (event-end event)))
	(window (posn-window (event-end event))))
    (set-buffer (window-buffer window))
    (goto-char pos)
    (camel-follow-link-at-point)))
      
(defface camel-face
'((t (:foreground "cyan" :underline "yellow")))
  "Face for CamelCase links.")

(defvar camel-face 'camel-face)

(defface camel-mouse-face
'((t (:background "peachpuff" :foreground "black")))
  "Face for mouseovered CamelCase links.")

(defvar camel-mouse-face 'camel-mouse-face)

(defvar camel-map nil)
(when (null camel-map)
  (setq camel-map (make-sparse-keymap))
  (define-key camel-map (kbd "<f9>" 'camel-mode)))

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
  (remove-text-properties (point-min) (point-max) '(camel-fontified))
  (font-lock-fontify-buffer))

(defvar *camel-mouse-map* (make-sparse-keymap))

(define-key *camel-mouse-map* [mouse-1] 'camel-mouse-follow-link)

(defun camel-render-link (begin end)
  (add-text-properties begin end `(camel-fontified ,(buffer-substring-no-properties begin end)
				   pointer hand
				   keymap ,*camel-mouse-map*
				   mouse-face camel-mouse-face
				   help-echo "Click to visit this wiki page.")))
				 
(defun camel-do-font-lock (add-or-remove)
  (funcall add-or-remove nil
	   `((,*camel-regexp* 0 (prog1 camel-face
				  (camel-render-link (match-beginning 0)
						     (match-end 0)))
			      prepend))))

(provide 'camel)
;;; camel.el ends here




