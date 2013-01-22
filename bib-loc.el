;;; bib-loc.el --- LOC (Library of Congress) Search and Retrieval BibTex

;; Copyright (C) 2013 Taichi Kawabata

;; Author: TK <kawabata.taichi@gmail.com>
;; Keywords: isbn, bibtex, library

;;; Commentary
;; 
;; Retrieve various bibliographic information in MARCXML format from
;; Library of Congress via SRU, and then convert it to bibtex format.
;;
;; References:
;;
;; http://www.loc.gov/standards/sru/
;; http://www.loc.gov/marc/
;; http://www.loc.gov/marc/bibliographic/ecbdhome.html

;; (defconst xml-entity-or-char-ref-re (concat "&\\(?:#\\(x\\)?\\([0-9a-f]+\\)\\|\\("
;;					    xml-name-re "\\)\\);"))


(require 'bib-xml)

(defvar bib-loc-query-url
  "http://z3950.loc.gov:7090/voyager?operation=searchRetrieve&version=1.1&startRecord=1&maximumRecords=10&query="
  "LOC SRU URL")

(defvar bib-loc-path-table
  '(;; 01X-09X: Numbers and Code Fields
    (leader     leader)
    (lccn       (datafield ((tag . "010"))) (subfield ((code . "a"))))
    (nbn1       (datafield ((tag . "015"))) (subfield ((code . "a"))))
    (nbn2       (datafield ((tag . "015"))) (subfield ((code . "2"))))
    (nbacn1     (datafield ((tag . "016") (ind1 . "7"))) (subfield ((code . "a"))))
    (nbacn2     (datafield ((tag . "016") (ind1 . "7"))) (subfield ((code . "2"))))
    (isbn       (datafield ((tag . "020"))) (subfield ((code . "a"))))
    (price      (datafield ((tag . "020"))) (subfield ((code . "c"))))
    (issn       (datafield ((tag . "022"))) (subfield ((code . "a"))))
    (strn       (datafield ((tag . "027"))) (subfield ((code . "a"))))
    ;; event date & place
    (eventdate  (datafield ((tag . "033"))) (subfield ((code . "a"))))
    (location   (datafield ((tag . "033"))) (subfield ((code . "p"))))
    ;; language
    (language   (datafield ((tag . "041"))) (subfield ((code . "a"))))
    (dewey      (datafield ((tag . "082"))) (subfield ((code . "a"))))
    (dewey-ed   (datafield ((tag . "082"))) (subfield ((code . "2"))))
    ;; 1XX - Main Entries-General Information
    (author1   (datafield ((tag . "100"))) (subfield ((code . "a")))) ; personal 
    ;(author-p  (datafield ((tag . "100"))) (subfield ((code . "d"))))
    (author2   (datafield ((tag . "110"))) (subfield ((code . "a")))) ; corporate
    (author3   (datafield ((tag . "111"))) (subfield ((code . "a")))) ; meeting
    ;; 20X-24X: Title and Title-Related Fields
    (title1     (datafield ((tag . "245"))) (subfield ((code . "a"))))
    (subtitle   (datafield ((tag . "245"))) (subfield ((code . "b"))))
    (titleaddon (datafield ((tag . "245"))) (subfield ((code . "c"))))
    (title2     (datafield ((tag . "245"))) (subfield ((code . "h"))))
    ;; 25X-28X: Edition, Imprint, Etc. Fields
    (edition    (datafield ((tag . "250"))) (subfield ((code . "a"))))
    (location   (datafield ((tag . "260"))) (subfield ((code . "a"))))
    (publisher  (datafield ((tag . "260"))) (subfield ((code . "b"))))
    (date       (datafield ((tag . "260"))) (subfield ((code . "c"))))
    ;; 3XX: Physical Description, Etc. Fields
    (extent1    (datafield ((tag . "300"))) (subfield ((code . "a")))) ; extent
    (extent2    (datafield ((tag . "300"))) (subfield ((code . "b")))) ; other physical
    (extent3    (datafield ((tag . "300"))) (subfield ((code . "c")))) ; dimension
    (extent4    (datafield ((tag . "300"))) (subfield ((code . "e")))) ; other material
    ;; 4XX: Series Statement Fields
    (series     (datafield ((tag . "490"))) (subfield ((code . "a"))))
    ;; 5XX: Note Fields
    (note0      (datafield ((tag . "500"))) (subfield ((code . "a")))) ;; general
    (note1      (datafield ((tag . "501"))) (subfield ((code . "a"))))
    (note2      (datafield ((tag . "502"))) (subfield ((code . "a"))))
    (note3      (datafield ((tag . "503"))) (subfield ((code . "a"))))
    (note4      (datafield ((tag . "504"))) (subfield ((code . "a")))) ;; bibliographic
    (note5      (datafield ((tag . "505"))) (subfield ((code . "a"))))
    (note6      (datafield ((tag . "506"))) (subfield ((code . "a"))))
    (note7      (datafield ((tag . "507"))) (subfield ((code . "a"))))
    (note8      (datafield ((tag . "508"))) (subfield ((code . "a"))))
    ;; 6xx - subject access fields
    (keywords0  (datafield ((tag . "650"))) (subfield ((code . "a")))) ;; topical
    (keywords1  (datafield ((tag . "651"))) (subfield ((code . "a")))) ;; topical
    (keywords3  (datafield ((tag . "653"))) (subfield ((code . "a")))) ;; topical
    (keywords4  (datafield ((tag . "654"))) (subfield ((code . "a")))) ;; topical
    (keywords5  (datafield ((tag . "655"))) (subfield ((code . "a")))) ;; topical
    ;; 70X-75X: Added Entry Fields
    (editor1    (datafield ((tag . "700"))) (subfield ((code . "a")))) ;; personal name
    (editor2    (datafield ((tag . "700"))) (subfield ((code . "d")))) ;; personal name
    (editor3    (datafield ((tag . "700"))) (subfield ((code . "4")))) ;; MARC Role
    (editor4    (datafield ((tag . "710"))) (subfield ((code . "a")))) ;; corporate name
    ;; 76X-78X: Linking Entry Fields
    ;; 80X-83X: Series Added Entry Fields
    (keywords6  (datafield ((tag . "830"))) (subfield ((code . "a"))))
    ;; 841-88X: Holdings, Location, Alternate Graphics, Etc. Fields
    (note9      (datafield ((tag . "880"))) (subfield ((code . "a"))))
    ))

;;; functions

;; temporary function 
(defun bib-xml-query-2 (query)
  "QUERY to the server with URL, then parse returned XML."
  (let* ((buffer 
          (url-retrieve-synchronously query)))
    (when buffer
      (switch-to-buffer buffer)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (while (re-search-forward "&#x\\([0-9a-fA-F]+\\);" nil t)
        (replace-match (char-to-string (string-to-number (match-string 1) 16))))
      (prog1
        (xml-parse-region (point-min) (point-max))
        (kill-buffer buffer)))))

(defun bib-loc-query (query)
  (bib-xml-query-2 (concat bib-loc-query-url query)))

;; M-x trace-function bib-loc-get-items
(defun bib-loc-get-items (query)
  "record dataの取得"
  (remove-if-not
   (lambda (x) (and (listp x) (equal (car x) 'record)))
   (bib-xml-get (bib-loc-query query)
                '(zs:searchRetrieveResponse zs:records zs:record zs:recordData))))

(defun bib-loc-parse-item (item)
  "recordデータから書誌情報を取り出し、ハッシュテーブルで返す。"
  (let* ((d (bib-xml-get item '(record)))
         (table (make-hash-table)))
    ;; `bib-loc-path-table' にある指定を順番に処理。
    (loop for key-path in bib-loc-path-table
          for key = (car key-path)
          for path = (cdr key-path)
          for val = (bib-xml-get d path)
          if val
          do (puthash key val table))
    (if bib-debug (message "debug: table=%s" table))
    table))

(defun bib-loc-process-bibdata (table)
  "TABLEの書誌情報を処理し、キー、タイプを計算してXMLを文字列に直す。"
  (let* (author title =key= =type=
         (leader (gethash 'leader table))
         (type1 (elt (car leader) 6))
         (type2 (elt (car leader) 7)))
    (remhash 'leader table)
    (setq =type= (case type1
                   ((?a ?c ?d) (case type2 
                                 ((?a ?m) "book")
                                 (?c "mvbook")))
                   ((?e ?f) "map")
                   (?g "video")
                   (?i "audio")
                   (?j "music")
                   (?k "image")
                   (?m "software")
                   ((?o ?p ?r ?t) "misc")))
    (case type2
      (?c (setq =type= "mvbook"))
      (?s (setq =type= "periodical")))
    (message "leaer=%s type1=%c type2=%c" leader type1 type2)
    (when (null =type=)
      (setq =type= "book"))
    ;; =key=
    (bib-aggregate-table-entry "nbn" table)
    (bib-aggregate-table-entry "nbacn" table)
    (bib-aggregate-table-entry "title" table)
    (bib-aggregate-table-entry "author" table)
    (bib-aggregate-table-entry "editor" table)
    (bib-aggregate-table-entry "note" table)
    (bib-aggregate-table-entry "extent" table)
    (bib-aggregate-table-entry "keywords" table)
    (setq author (gethash 'author table)
          title  (gethash 'title table))
    (setq =key= (concat (bib-first-word (car author)) "_" (bib-first-word (car title))))
    (message "key=%s type=%s" =key= =type=)
    (puthash '=type= (list =type=) table)
    (puthash '=key= (list =key=) table)
    table))

(defun bib-loc (query)
  "Queryに従ったBibTeXデータのリストを返す。"
  (let* ((items (bib-loc-get-items query)))
    (mapcar (lambda (item)
              (bib-entry
               (bib-loc-process-bibdata
                (bib-loc-parse-item (list item)))))
            items)))

(defun bib-loc-query-buffer (query)
  (switch-to-buffer (get-buffer-create "*BibLaTeX*"))
  (bibtex-mode)
  (goto-char (point-max))
  (mapcar (lambda (x) (insert x "\n\n")) (bib-loc query)))

(defun bib-loc-bib-buffer (title &optional creator) ;; year
  "Search by author and creator."
  (interactive 
   (list
    (read-string "Title　 ? " bib-xml-title)
    (read-string "Creator ? " bib-xml-creator)
    ;;(read-string "Year (e.g. 2012-, 1945-1960, etc.)? " bib-xml-year)
    ))
  (let* (result query from until
         ;(year (or year ""))
         )
      (if (< 0 (length title)) (push (concat "title any \"" title "\" ") result))
      (if (< 0 (length creator)) (push (concat "creator any \"" creator "\"") result))
      ;(if (string-match "^\\([0-9]+\\)?-\\([0-9]+\\)?$" year)
      ;    (setq from  (match-string 1 year)
      ;          until (match-string 2 year)))
      ;(if (string-match "^\\([0-9]+\\)?$" year)
      ;    (setq from  (match-string 1 year)
      ;          until (match-string 1 year)))
      ;(if (/= 0 (length from))
      ;    (push (format "year_from=%s" from) result))
      ;(if (/= 0 (length until))
      ;    (push (format "year_to=%s" until) result))
      (setq query (concat "(" (mapconcat 'identity result " and ") ")"))
      (bib-loc-query-buffer query)))

;;; bib-loc.el ends here.

(provide 'bib-loc)
