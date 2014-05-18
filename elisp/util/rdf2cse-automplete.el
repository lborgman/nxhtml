;;; rdf2cse-automplete.el --- 
;; 
;; Filename: rdf2cse-automplete.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: 2013-12-21T04:51:46+0100
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defun gcse-autocomplete-from-zotero-rdf()
  "Extract autocomplete information from Zotero for Google CSE.
To use this first make an export of the Zotero library in RDF-format.

Then open that file and run this routine. The output will be in
the format expected by Google CSE for autocomplete."
  (interactive)

  (message "- Find all titles, abstracts and tags")
  (goto-char 0)
  (let* ((tags1 '(
                 "<dc\:title>"          ;; Title
                 ;; "<dcterms\:abstract>"  ;; Abstract
                 "<rdf:value>"          ;; Notes, but also URLs (which will be removed later)
                 "<foaf:surname>"       ;; Surname
                 "<dc:subject>"         ;; Tags
                 ))
         (pattern (regexp-opt tags1))
         (tags2 (mapcar (lambda(e)
                          (concat "</" (substring e 1)))
                        tags1))
         (pattern2 (regexp-opt tags2))
         )
    (delete-non-matching-lines pattern)

    (message "- Delete other titles")
    (goto-char 0)
    ;;(delete-matching-lines "^           ")
    (goto-char 0)
    (delete-matching-lines "><dc.title")
    (message "- Free lines from xml")
    (goto-char 0)
    ;;(replace-regexp "^.*?<dc:title>" "")
    (replace-regexp (concat "^.*?" pattern) "")
    (goto-char 0)
    ;; (replace-regexp "</dc:title>.*$" "")
    (replace-regexp (concat pattern2 ".*$") "")
    )

  (message "- Delete strange characters etc")
  (goto-char 0)
  (replace-regexp "&quot;" "")
  (goto-char 0)
  ;; (replace-regexp "[\"'.,]" " ")

  (message "- Split sentences")
  (goto-char 0)
  (while (not (eobp))
    (let ((end (line-end-position)))
      (skip-chars-forward "^.?!:" end)
      ;; (read-char (format "look after skip, end=%s" end))
      (unless (eq (point) end)
        (let ((split t))
          (when (looking-at ":/") (setq split nil))
          (when (looking-at ":[0-9]") (setq split nil))
          (when (looking-at "\.[0-9]") (setq split nil))
          ;; (when (looking-back "") (setq split nil))
          (when split
            ;; (read-char "look splitting")
            (delete-char 1)
            (insert "\n")
            ;; (read-char "look after splitting")
            ;; Some bug in skip, try updating by moving back one line. Works!
            (forward-line -1)
            ;; (error "stop")
            )
        )))
    (forward-line))

  (message "- Delete Zotero HTML tags")
  (goto-char 0)
  (replace-regexp "&lt;.+?>" "")
    
  (message "- Trim")
  (goto-char 0)
  (replace-regexp "^[ ]+" "")
  (goto-char 0)
  (replace-regexp "[ ]+$" "")

  (message "- Delete some things not interesting here")
  (goto-char 0)
  ;; (delete-matching-lines "^[^ ]*$")
  (goto-char 0)
  (delete-matching-lines "^amazon\.com ")
  (goto-char 0)
  (delete-matching-lines "^annotated ")
  (goto-char 0)
  (delete-matching-lines "^discussion in facebook group ")
  (goto-char 0)
  (delete-matching-lines "^discussion in fb group ")
  (goto-char 0)
  (delete-matching-lines "^full article")
  (goto-char 0)
  (delete-matching-lines "^full book")
  (goto-char 0)
  (delete-matching-lines "^full text")
  (goto-char 0)
  (delete-matching-lines "^google books link")
  (goto-char 0)
  (delete-matching-lines "^http.?://")
  (goto-char 0)
  (delete-matching-lines "^in facebook group ")
  (goto-char 0)
  (delete-matching-lines "^news article")
  (goto-char 0)
  (delete-matching-lines "^pubmed central link")
  (goto-char 0)
  (delete-matching-lines "^pubmed entry")
  (goto-char 0)
  (delete-matching-lines "^pubmed link")
  (goto-char 0)
  (delete-matching-lines "\.htm$")
  (goto-char 0)
  (delete-matching-lines "\.html$")
  (goto-char 0)
  (delete-matching-lines "\.pdf$")

  (message "- Sort")
  (let ((sort-fold-case t))
    (sort-lines nil 1 (buffer-size())))
  ;; (error "stop")

  (message "- Shorten long lines")
  (goto-char 0)
  (let ((maxlen (length "Cognitive Behavioral Analysis System of Psychotherapy and Brief Supportive make lines longer and1234")))
    (while (not (eobp))
      (let ((len (- (point-at-eol) (point))))
        (when (> len maxlen)
          (forward-char maxlen)
          (skip-chars-backward "^ ")
          (delete-region (1- (point)) (line-end-position))
          (forward-line -1) ;; To avoid bogus line length
          ))
      (forward-line)))

  (message "- Delete short lines. And strange lines.")
  (goto-char 0)
  (while (not (eobp))
    (let ((len (- (point-at-eol) (point))))
      (if (or (< len 3)
              (memq (char-after) (append ".:)," nil)))
          (delete-region (point) (1+ (point-at-eol)))
        (forward-line))))
  
  
  (message "- Remove duplicates")
  (goto-char 0)
  (while (re-search-forward "^\\(.*\n\\)\\1+" nil t)
    (replace-match "\\1"))
  
  (message "- Make XML")
  (when t
    (let ((name (buffer-file-name)))
      (write-file (concat name "-temp-autocomplete.txt"))
      (goto-char 0)
      (while (not (eobp))
        (insert "<Autocompletion term=\"")
        (end-of-line)
        (insert "\" type=\"1\" language=\"\" />")
        (forward-line))
      (insert "\n</Autocompletions>\n")
      (goto-char 0)
      (insert "<Autocompletions>\n")
      (goto-char 0)
      (write-file (concat name "-temp-autocomplete.xml"))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rdf2cse-automplete.el ends here
