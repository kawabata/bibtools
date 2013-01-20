;;; bib-xml.el --- XML tools for Bibliography

;; Copyright (C) 2013 Taichi Kawabata

;; Author: Taichi Kawabata <kawabata.taichi at gmail.com>
;; Keywords: isbn, bibtex, library

;;; Commentary
;;
;; This is a library for handling XML data suitable for bibliographic
;; RDF analysis.

(require 'bibtex)

(defvar bib-debug t)

(defun bib-xml-query (query)
  "QUERY to the server with URL, then parse returned XML."
  (let* ((buffer 
          (url-retrieve-synchronously query)))
    (when buffer
      (switch-to-buffer buffer)
      (set-buffer-multibyte t)
      (prog1
          (xml-parse-region (point-min) (point-max))
        (kill-buffer buffer)))))

(defun bib-xml-get (xmls path)
  "Collect xml node from XMLs specified by PATH.
PATH is a list consists of element, attributes and regexp."
  (let ((result xmls) elem attrs regexp collect-attrs)
    (dolist (spec path)
      (if (listp spec) 
          (setq elem (car spec) 
                attrs (cadr spec) 
                regexp (caddr spec)
                collect-attrs (cadddr spec))
        (setq elem spec))
      (setq result (bib-xml-get-each result elem attrs regexp collect-attrs))
      (setq elem nil attrs nil regexp nil))
    result))

(defun bib-xml-get-each (xmls elem &optional attrs regexp collect-attrs)
  "Retrieve children of elements specified by ELEM, and ATTRS.
When REGEXP is specified, element value is checked against that
regexp and if it maches, return the value of `(match-string 1)'.
When COLLECT-ATTRS is t, then collect attributes rather than values."
  (setq xmls (remove-if 'stringp xmls))
  (let ((result
         (loop for xml in xmls
               for elem2 = (car xml)
               for attrs2 = (cadr xml)
               for body2 = (cddr xml)
               if (and (equal elem elem2)
                       (or (null attrs)
                           (bib-xml-compare-attrs attrs attrs2)))
               append (if collect-attrs attrs2 body2)
               ))
        result2)
    (if (null regexp) result
      (dolist (x result)
        (if (and (stringp x) (string-match regexp x))
            (setq result2 (cons (match-string 1 x) result2))))
      result2)))

(defun bib-xml-compare-attrs (attrs attrs2)
  "Compare ATTRS and ATTRS2.  If ATTRS2 has all the attributes
specified with ATTRS, with its values matches to regexp of ATTRS,
then return t.  Otherwise, return nil."
  (if attrs2
      (loop for (name . val) in attrs
            for val2 = (assoc-default name attrs2)
            always (string-match val val2))))

;;; misc functions

(defun bib-entry (table)
  "create bibtex entry from hashTABLE."
  (with-temp-buffer
    (let (bibtex-files) ;; make this variable empty to avoid key checki.
      (bibtex-mode)
      (insert "@" (car (gethash '=type= table)) "{" (car (gethash '=key= table)))
      (maphash (lambda (name val) 
        (unless (member name '(=key= =type=))
          (insert ",\n" (symbol-name name) "={" (bib-concat val) "}")))
               table)
      (insert "\n}")
      (goto-char (point-min))
      (bibtex-clean-entry)
      (buffer-string))))

(defun bib-concat (list)
  (mapconcat 'identity list " and "))

(defun bib-first-word (text)
  "Retrieve first word of text, without symbolic_punctuational characters U+2000-U+303F"
  (when text
    (with-temp-buffer 
      (insert (ucs-normalize-NFKC-string text))
      (goto-char (point-min))
      (while (re-search-forward "[,\" -〿]" nil t) (replace-match ""))
      (goto-char (point-min))
      (forward-word)
      (buffer-substring (point-min) (point)))))

;; ISBN

(defun bib-normalize-isbn (isbn)
  "Convert ISBN to ISBN13 w/o hyphen."
  (let ((isbn (replace-regexp-in-string "-" "" isbn)))
    (if (string-match "^[0-9]\\{9\\}[0-9X]$" isbn)
        (bib-isbn13 isbn)
      (if (string-match "^[0-9]\\{13\\}$" isbn) isbn
        (error "Not proper ISBN format! -- %s" isbn)))))

(defun bib-isbn13 (isbn)
  "Convert ISBN10 to ISBN13.  Return nil otherwise."
  (if (string-match "^[0-9]\\{9\\}[0-9X]$" isbn)
      (let ((sum 0)
            (isbn (substring (concat "978" isbn) 0 -1)))
        (do ((item (string-to-list isbn) (cdr item))
             (chck '(1 3 1 3 1 3 1 3 1 3 1 3) (cdr chck)))
            ((null chck))
          (setq sum (+ sum (* (- (car item) ?0) (car chck)))))
        (setq sum (mod sum 10))
        (concat isbn (if (= sum 0) "0" (number-to-string (- 10 sum)))))))

(defun bib-isbn-to-10 (isbn)
  "Convert ISBN13 to ISBN10.  Return nil otherwise."
  (if (string-match "^978[0-9]\\{10\\}$" isbn)
      (let ((sum 0)
            (isbn (substring isbn 3 -1)))
        (do ((item (string-to-list isbn) (cdr item))
             (chck '(10 9 8 7 6 5 4 3 2) (cdr chck)))
            ((null chck))
          (setq sum (+ sum (* (- (car item) ?0) (car chck)))))
        (setq sum (- 11 (mod sum 11)))
        (concat isbn (if (= sum 11) "0" (if (= sum 10) "X" (number-to-string sum)))))))

(provide 'bib-xml)
