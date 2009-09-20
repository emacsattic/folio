;;; folio.el --- GNOME-integrated notetaking system built on Org

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: tools, hypermedia

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

;; 

;;; Code:

(require 'cl)
(require 'org)
(require 'camel)
(require 'uuidgen)

;;; Notebooks

;; A notebook is a named collection of files, plus a list of
;; property->value metadata pairs used to describe the folder's
;; contents and control its behavior. Notebooks are self-contained and
;; movable, so a notebook's current disk location is not one of its
;; properties. Instead we cache the folder name and the
;; currently-scanned list of files.

(defstruct folio-notebook 
  properties ;; written to the .folio file
  files ;; cached list of files in this group. absolute names are used.
  folder ;; disk directory of database
  )

;;; Including, excluding, finding files in notebooks

(defun folio-match-regexp-or-list (filename regexp-or-list)
  "Return non-nil if FILENAME is either a member of the list (or
matches the regexp) REGEXP-OR-LIST. Return t if there is nothing
to match."
  (if (null regexp-or-list)
      t
      (etypecase regexp-or-list
	(string (string-match regexp-or-list filename))
	(list (member filename regexp-or-list)))))

(defun* folio-file (file &optional (notebook *folio-current-notebook*))
  "Find FILE within the notebook NOTEBOOK."
  (expand-file-name file (folio-notebook-folder (folio-find-notebook notebook))))

(defun folio-filter-files (files regexp-or-list)
  "Return FILES with any matching files removed.  If
REGEXP-OR-LIST is a regular expression, remove files matching the
regexp. If it's a list, remove files matching any filename in the
list. See also `folio-match-regexp-or-list'."
  (labels ((match (filename)
	     (folio-match-regexp-or-list filename regexp-or-list)))
    (remove-if #'match files)))

(defun* folio-get-files (&optional (notebook *folio-current-notebook*))
  "Obtain a list of all the available files in the notebook NOTEBOOK."
  (let ((folder (folio-notebook-folder notebook)))
    (labels ((expand (filename)
	       (expand-file-name filename folder)))
      (let* ((include (fprop :include notebook))
	     (exclude (fprop :exclude notebook))
	     (files (etypecase include
		      (string (directory-files folder nil include))
		      (list (if include
				;; if :include property is not
				;; specified, all files are returned.
				(mapcar #'expand include)
				(directory-files folder))))))
	(remove-if #'file-directory-p
		   (mapcar #'expand 
			   (remove-duplicates 
			    (folio-filter-files files exclude)
			    :test 'equal)))))))

(defun* folio-scan-files (&optional notebook)
  "Update the list of cached filenames for the notebook NOTEBOOK."
  (interactive)
  (let* ((book (folio-find-notebook notebook))
	 (name (fprop :name book)))
    (message "Scanning notebook %s for files..." name)
    (let ((files (setf (folio-notebook-files book)
		       (folio-get-files book))))
      (message "Scanning notebook %s for files... Done." name)
      (message "Found %d files." (length files)))))

;;; Notebook properties

;; Some properties a notebook can have: 

;; :name    String name of group. 
;; :uuid    Universally Unique Identifier string
;; :tags    List of keyword symbols
;; :last-updated-date    ISO Date string
;; :base-directory    When non-nil, override project's location.
;; :include    When non-nil, a regexp or list of filenames to include.
;; :exclude    When non-nil, a regexp or list of files to exclude.
;; :publishing-configuration TODO etags, etc

(defvar *folio-default-properties* '(:exclude "^\\."))

(defvar *folio-properties-file-name* ".folio")

(defun* folio-notebook-property (property &optional (notebook *folio-current-notebook*))
  (or (getf (folio-notebook-properties (folio-find-notebook notebook)) property)
      (getf *folio-default-properties* property)))

(defun* folio-set-notebook-property (property notebook value)
  (setf (getf (folio-notebook-properties (folio-find-notebook notebook)) property)
	value))

(defalias 'fprop 'folio-notebook-property)

(defsetf fprop folio-set-notebook-property)

;;; Loading and saving properties to disk

(defun folio-write-sexp-to-file (filename sexp)
  (with-temp-buffer
    (insert (format "%S" sexp))
    (write-file filename)
    (kill-buffer)
    (message "Wrote folio properties data to %s" filename)))

(defun folio-read-sexp-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (prog1 (read (current-buffer))
      (message "Read folio properties data from %s" filename))))

(defun* folio-write-properties (properties 
				&optional (notebook *folio-current-notebook*))
  (let ((book (folio-find-notebook notebook)))
    (folio-write-sexp-to-file (folio-file *folio-properties-file-name* book)
			      (folio-notebook-properties book))))

(defun* folio-read-properties (&optional (notebook *folio-current-notebook*))
  (let* ((book (folio-find-notebook notebook))
	 (propsfile (folio-file *folio-properties-file-name* book)))
    (when book
	;; create blank propsfile if necessary
      (when (not (file-exists-p propsfile))
	(with-temp-buffer 
	  (insert "()")
	  (write-file propsfile)))
      ;; now read properties
      (setf (folio-notebook-properties book)
	    (folio-read-sexp-from-file propsfile)))))

;;; The user's notebook table

(defvar *folio-notebooks* nil
  "Hash table mapping string names to notebook objects.")

(defvar *folio-current-notebook* nil
  "Name of the selected notebook, if any.")

(defun folio-initialize-notebooks ()
  (setf *folio-notebooks* (make-hash-table :test 'equal)))

(defun folio-choose-notebook ()
  (interactive)
  (completing-read "Choose notebook: " *folio-notebooks* nil :require-match))

(defun* folio-switch-to-notebook (&optional (name (folio-choose-notebook)))
  (interactive)
  (if (gethash name *folio-notebooks*)
      (progn (setf *folio-current-notebook* name)
	     (message "Switched to notebook %s" name))
      (error "Notebook not found: %s" name)))

;;; Creating and opening notebooks.

(defun folio-add-notebook (notebook)
  (assert (folio-notebook-p notebook))
  (let ((name (fprop :name notebook)))
    (when (gethash name *folio-notebooks*)
      ;; resolve name conflicts by appending UUID to table key.
      ;; this only affects the user's table, it does not affect the notebook.
      (setf name (concatenate 'string name (fprop :uuid notebook))))
    (setf (gethash name *folio-notebooks*) notebook)
    (message "Added notebook %s" notebook)))
			 
(defun folio-load-notebook (directory)
  (let* ((folder (file-name-as-directory directory))
	 (book (make-folio-notebook :folder folder)))
    (prog1 book
      (folio-read-properties book)
      (folio-scan-files book)
      (folio-add-notebook book))))

(defun* folio-find-notebook (&optional (notebook *folio-current-notebook*))
  (when (null *folio-notebooks*)
    (folio-initialize-notebooks))
  (cond ((folio-notebook-p notebook)
	 notebook)
	((stringp notebook)
	 (gethash notebook *folio-notebooks*))
	(t (error "Cannot find notebook %S" notebook))))

(defun* folio-create-notebook (properties folder)
  ;; create folder if required
  (unless (and (file-exists-p folder)
	       (file-directory-p folder))
    (make-directory folder :parents)
    (message "Created directory %s for new notebook." folder))
  (let ((book (make-folio-notebook :properties properties 
				   :folder folder)))
    (prog1 book
      (when (null (fprop :uuid book))
	(setf (fprop :uuid book) (make-uuid)))
      (folio-write-properties properties book)
      (folio-scan-files book)
      (folio-add-notebook book))))

;; (folio-create-notebook '(:name "FolioTest") "~/ftest")
;; (folio-load-notebook "~/ftest")

;;; Folio frames

;; Using these font names as a default assumes recent Emacs, which can
;; interpret the new-style font names.

(defvar *folio-monospace-font* "Monospace 8") 

(defvar *folio-sans-font* "Sans 8")

;; (defun folio-configure-frame

;; (defun folio-popup 


(provide 'folio)
;;; folio.el ends here
