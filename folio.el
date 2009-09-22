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
  folder ;; disk directory of database.
  pinned ;; non-nil when notebook is to be retained across sessions.
  ;; otherwise, you must manually re-open the notebook next
  ;; session with M-x `folio-open-notebook'.
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

(defvar *folio-default-properties* '(:exclude "^\\.")
  "These default property values may be overridden by setting the
relevant notebook property. See also `folio-notebook-property'.")

(defvar *folio-properties-file-name* ".folio"
  "File name for storing a notebook's property data.")

(defun* folio-notebook-property (property &optional (notebook *folio-current-notebook*))
  "Read the value of the PROPERTY. Optionally specify which
NOTEBOOK to read it from."
  (or (getf (folio-notebook-properties (folio-find-notebook notebook)) property)
      (getf *folio-default-properties* property)))

(defun* folio-set-notebook-property (property notebook value)
  "Set VALUE as the value of the property PROPERTY in NOTEBOOK."
  (setf (getf (folio-notebook-properties (folio-find-notebook notebook)) property)
	value))

(defalias 'fprop 'folio-notebook-property)

(defsetf fprop folio-set-notebook-property)

;;; Loading and saving properties to disk

(defun folio-write-sexp-to-file (filename sexp)
  "Write a #'read-able representation of SEXP to FILENAME."
  (with-temp-buffer
    (insert (format "%S" sexp))
    (write-file filename)
    (kill-buffer)))

(defun folio-read-sexp-from-file (filename)
  "Read a single s-expression from the text file FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (read (current-buffer))))

(defun* folio-write-properties (properties 
				&optional (notebook *folio-current-notebook*))
  "Write the PROPERTIES list to the folio properties file in NOTEBOOK."
  (let* ((book (folio-find-notebook notebook))
	 (propsfile (folio-file *folio-properties-file-name* book)))
    (folio-write-sexp-to-file propsfile
			      (folio-notebook-properties book))
    (message "Wrote notebook properties data to %s" propsfile)))

(defun* folio-read-properties (&optional (notebook *folio-current-notebook*))
  "Read the NOTEBOOK's properties from its folio properties file."
  (let* ((book (folio-find-notebook notebook))
	 (propsfile (folio-file *folio-properties-file-name* book)))
    (when book
	;; create blank propsfile if necessary
      (when (not (file-exists-p propsfile))
	(with-temp-buffer 
	  (insert "()")
	  (write-file propsfile)))
      ;; now read properties
      (prog1 (setf (folio-notebook-properties book)
		   (folio-read-sexp-from-file propsfile))
      (message "Read notebook properties data from %s" propsfile)))))

;;; The user's notebook table

;; Your notebook table is an index of all your open notebooks.
;; Notebooks can be "pinned"; this means it will be kept open across
;; sessions (i.e. when you restart Emacs.)

(defvar *folio-notebooks* nil
  "Hash table mapping string names to notebook objects.")

(defvar *folio-current-notebook* nil
  "Name of the selected notebook, if any.")

(defvar *folio-notebook-table-file* "~/.folio-notebooks"
  "Default file location of saved notebook table.")

(defun* folio-prune-notebook-table (&optional (table *folio-notebooks*))
  "Close all non-pinned notebooks."
  (interactive)
  (labels ((prune (name notebook)
	     (unless (folio-notebook-pinned notebook)
	       (folio-close-notebook notebook))))
    (maphash #'prune table)))

(defun* folio-rescan-notebook (&optional (notebook *folio-current-notebook*))
  (let* ((book (folio-find-notebook notebook))
	 (dir (folio-notebook-folder book))
	 (propsfile (folio-file *folio-properties-file-name* book)))
    (if (and (file-exists-p dir)
	     (file-directory-p dir)
	     (file-exists-p propsfile))
	(prog1 (folio-read-properties book)
	  (folio-scan-files book)
	  (message "Rescanned notebook %s and found %d files."
		   (fprop :name book)
		   (length (folio-notebook-files book))))
	(message "WARNING: Failed to scan notebook %s in directory %s. Removing notebook." (fprop :name book) dir))))

(defun* folio-rescan-all-notebooks (&optional (table *folio-notebooks*))
  (interactive)
  (labels ((rescan (name book)
	     (folio-rescan-notebook book)))
    (maphash #'rescan table)))
	     
(defvar *folio-crash-recovering-p* nil)

(defun folio-read-notebook-table-maybe ()
  (let ((file *folio-notebook-table-file*))
    (when (file-exists-p file)
      (if (featurep 'hashtable-print-readable)
	  (let ((table (folio-read-sexp-from-file file)))
	    (prog1 table
	      (assert (hash-table-p table))
	      (unless *folio-crash-recovering-p* 
		(folio-prune-notebook-table table))
	      (folio-rescan-all-notebooks table)))
	  (message "Not loading notebook table due to lack of printable hashes.")))))

(defun folio-write-notebook-table-maybe ()
  (let ((file *folio-notebook-table-file*))
    (if (featurep 'hashtable-print-readable)
	(folio-write-sexp-to-file file *folio-notebooks*)
	(message "Not saving notebook table due to lack of printable hashes."))))

(defun folio-initialize-notebooks ()
  (setf *folio-notebooks* (or (folio-read-notebook-table-maybe)
			      (make-hash-table :test 'equal))))

(defun folio-choose-notebook ()
  (interactive)
  (completing-read "Choose notebook: " *folio-notebooks* nil t))

(defun* folio-switch-to-notebook (&optional (name (folio-choose-notebook)))
  (interactive)
  (if (gethash name *folio-notebooks*)
      (progn (setf *folio-current-notebook* name)
	     (message "Switched to notebook %s" name))
      (error "Notebook not found: %s" name)))

(defun folio-add-notebook (notebook)
  (assert (folio-notebook-p notebook))
  (let ((name (fprop :name notebook)))
    ;; (when (gethash name *folio-notebooks*)
    ;;   ;; resolve name conflicts by appending UUID to table key.
    ;;   ;; this only affects the user's table, it does not affect the notebook.
    ;;   (setf name (concat name (fprop :uuid notebook))))
    (setf (gethash name *folio-notebooks*) notebook)
    (folio-write-notebook-table-maybe)
    (message "Added notebook %s" notebook)))
			 
(defun* folio-open-notebook (directory &optional pinned)
  "Add the notebook in DIRECTORY to the user's notebook table.
When PIN is non-nil, the notebook will be pinned (i.e. marked as
automatically opened on the next session)."
  (let* ((folder (file-name-as-directory directory))
	 (book (make-folio-notebook :folder folder :pinned pinned)))
    (unless (and (file-exists-p folder)
		 (file-directory-p folder))
      (error "Cannot open directory %s" directory))
    (prog1 book
      (folio-read-properties book)
      (folio-scan-files book)
      (folio-add-notebook book))))

(defun* folio-close-notebook (&optional (notebook *folio-current-notebook*))
  (remhash notebook *folio-notebooks*)
  (when (equal notebook *folio-current-notebook*)
    (setf *folio-current-notebook* nil)))
;; TODO close buffers visiting those files? 

(defun* folio-find-notebook (&optional (notebook *folio-current-notebook*))
  (when (null *folio-notebooks*)
    (folio-initialize-notebooks))
  (cond ((folio-notebook-p notebook)
	 notebook)
	((stringp notebook)
	 (or (gethash notebook *folio-notebooks*)
	     (error "Unable to find notebook %s" notebook))) 
	(t (error "Invalid notebook."))))

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

;;; Folio pages (org files in a notebook)

(defvar *folio-default-page-name* "FrontPage")

(defvar *folio-page-extension* ".org")

(defun* folio-find-page (&optional (page *folio-default-page-name*)
				   (notebook *folio-current-notebook*))
  (find-file (folio-file (concat page *folio-page-extension*) notebook)))
   
;;; Folio frames

(require 'iimage)

(defvar *folio-use-inline-images* t
  "User configuration for inline images.")

(make-variable-buffer-local (defvar *folio-inline-images* nil))

(defvar *folio-images-regexp* (concat "file:\\(~?" 
				      iimage-mode-image-filename-regex
				      "\\)"))

(defun* folio-do-inline-images (arg)
  (let ((buffer-read-only nil)
	(file nil)
	(modified-p (buffer-modified-p (current-buffer))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward *folio-images-regexp* nil t)
	(if (and (setf file (folio-file (match-string 1)))
		 (file-exists-p file))
	    (cond ((and (numberp arg) (plusp arg))
		   (progn (add-text-properties (match-beginning 0)
					       (match-end 0)
					       (list 'display (create-image file)))
			  (setf *folio-inline-images* t)))
		  ((null arg)
		   (if (null *folio-inline-images*)
		       (progn (add-text-properties (match-beginning 0)
						   (match-end 0)
						   (list 'display (create-image file)))
			      (setf *folio-inline-images* t))
		       (progn (remove-text-properties (match-beginning 0)
						      (match-end 0)
						      '(display))
			      (setf *folio-inline-images* nil))))))))
    ;; restore mod status
    (set-buffer-modified-p modified-p)))

(defun* folio-toggle-inline-images (&optional arg)
  "Toggle visibility of org image links in current buffer."
  (interactive)
  (let ((flag (if (numberp arg)
		  (if (plusp arg) nil t)
		  (if *folio-inline-images* t nil))))
    (set-face-underline-p 'org-link flag)
    (folio-do-inline-images arg)))

;; Using these font names as a default assumes recent Emacs, which can
;; interpret the new-style font names.

(defvar *folio-monospace-font* "Monospace 8") 

(defvar *folio-sans-font* "Sans 8")

(defvar *folio-use-theme* t)

(defvar *folio-use-menu-bar* t)

(defvar *folio-use-tool-bar* nil)

(defun* folio-set-buffer-font (&optional (font *folio-sans-font*))
  (buffer-face-set (font-face-attributes font)))

(defun* folio-configure-frame (&optional (frame (selected-frame)))
  (with-selected-frame frame
    ;; (scroll-bar-mode -1)
    (set-frame-parameter frame 'menu-bar-lines 
			 (if *folio-use-menu-bar* 1 nil))
    (set-frame-parameter frame 'tool-bar-lines 
			 (if *folio-use-tool-bar* 1 nil))
    (when *folio-use-theme* (color-theme-folio))))

(defun* folio-configure-buffer (&key (buffer (current-buffer))
				     (font *folio-sans-font*))
  (with-current-buffer buffer
    (setf mode-line-format nil)
    (folio-set-buffer-font font)
    (local-set-key (kbd "<f8>") 'folio-toggle-inline-images)
    (folio-toggle-inline-images 1)
    (folio-update-header-line)
    (camel-mode 1)))

(defun* folio-make-frame-on-page (&key (page *folio-default-page-name*)
				       (notebook *folio-current-notebook*))
  (interactive)
  (let ((frame (make-frame)))
    (select-frame frame)
    (folio-configure-frame frame)
    (folio-find-page page notebook)
    (folio-configure-buffer)))

;;; Header line

(defun folio-update-header-line ()
  (setf header-line-format 
	(concat 
	 "File: "
	 (propertize (file-name-nondirectory (buffer-file-name (current-buffer)))
		     'face 'font-lock-function-name-face)
	 " Notebook: "
	 (propertize *folio-current-notebook* 
		     'face 'font-lock-variable-name-face))))

;;; Tests

;; (setf *folio-notebooks* (make-hash-table :test 'equal))

;; (delete-file *folio-notebook-table-file*)
;; (setf *folio-notebooks* nil)
;; (folio-initialize-notebooks)
;; (folio-write-notebook-table-maybe)

;; (folio-open-notebook "~/folio/example")
;; (folio-switch-to-notebook "Example Notebook")
;; (folio-make-frame-on-page :notebook "Example Notebook")
;; (folio-close-notebook "Example Notebook")

;; (folio-create-notebook '(:name "Example Notebook") "~/folio/example")
;; (folio-set-buffer-font *folio-monospace-font*)
;; (folio-set-buffer-font *folio-sans-font*)


(provide 'folio)
;;; folio.el ends here
