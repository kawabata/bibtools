;;; bib-cinii.el --- CiNii access 

;; Copyright (C) 2013 Taichi Kawabata

;; Author: Taichi Kawabata <kawabata.taichi at gmail.com>
;; Keywords: isbn, bibtex, library

;;; Commentary
;; 
;; CiNii から様々な書誌情報を取り出し、BibLaTeXで出力する。
;;
;; 参考文献
;; - http://ci.nii.ac.jp/info/ja/api/a_opensearch.html

(require 'bib-xml)

(defvar bib-cinii-query-url
  "http://ci.nii.ac.jp/opensearch/search?format=rss&count=50&"
  "CiNii 問い合わせURL。format=rssとすること。")

(defvar bib-cinii-path-table
  '(
    (title        title)
    (url          link)
    (author       dc:creator)
    (publisher    dc:publisher)
    (journaltitle prism:publicationName)
    (issn         prism:issn)
    (volume       prism:volume)
    (number       prism:number)
    (start        prism:startingPage)
    (end          prism:endingPage)
    (page         prism:pageRange)
    (abstract     description)
    (year         dc:date)
    ))

;;; functions

(defun bib-cinii-query (query)
  (bib-xml-query (concat bib-cinii-query-url query)))

;; M-x trace-function to debug.
(defun bib-cinii-get-items (query)
  "CiniiのRDFデータから、itemのリストを取り出す。"
  ;; query は、recordPacking=xml とすること。
  (remove-if-not
   (lambda (x) (and (listp x) (equal (car x) 'item)))
   (bib-xml-get (bib-cinii-query query) '(rdf:RDF))))

(defun bib-cinii-parse-item (item)
  "itemデータから書誌情報を取り出し、ハッシュテーブルで返す。"
  (let* ((d (bib-xml-get item '(item)))
         (table (make-hash-table)))
    ;; `bib-cinii-path-table' にある指定を順番に処理。
    (loop for key-path in bib-cinii-path-table
          for key = (car key-path)
          for path = (cdr key-path)
          for val = (bib-xml-get d path)
          if val
          do (puthash key val table))
    (if bib-debug (message "debug: table=%s" table))
    table))

(defun bib-cinii-process-bibdata (table)
  "TABLEの書誌情報を処理し、キー、タイプを計算してXMLを文字列に直す。"
  (let* ((author    (gethash 'author table))
         (title     (gethash 'title table))
         (start     (gethash 'start table))
         (end       (gethash 'end table))
         =key= =type=)
    ;; =key=
    (message "author=%s title=%s" author title)
    (setq =key= (concat (bib-first-word (car author)) "_" (bib-first-word (car title))))
    (setq =type= "article")
    (when (or start end)
      (puthash 'pages (list (concat (car start) "--" (car end))) table)
      (remhash 'start table)
      (remhash 'end table))
    (puthash '=type= (list =type=) table)
    (puthash '=key= (list =key=) table)
    table))

(defun bib-cinii (query)
  "Queryに従ったBibTeXデータのリストを返す。"
  (let* ((items (bib-cinii-get-items query)))
    (mapcar (lambda (item)
              (bib-entry
               (bib-cinii-process-bibdata
                (bib-cinii-parse-item (list item)))))
            items)))

(defun bib-cinii-query-buffer (query)
  (switch-to-buffer (get-buffer-create "*BibLaTeX*"))
  (bibtex-mode)
  (goto-char (point-max))
  (mapcar (lambda (x) (insert x "\n\n")) (bib-cinii query)))

;;###autoload
(defun bib-cinii-bib-buffer (title &optional creator year)
  "タイトル・著者（訳者）・出版社・発行年・種類（本・記事）などから検索する。"
  (interactive 
   (list
    (read-string "タイトル　 ? " bib-xml-title)
    (read-string "著者・訳者 ? " bib-xml-creator)
    (read-string "発行年　　 (e.g. 2012-, 1945-1960, etc.)? " bib-xml-year)))
  (let* (result query from until
         (year (or year "")))
      (if title (push (concat "title=" title) result))
      (if creator (push (concat "author=" creator) result))
      (if (string-match "^\\([0-9]+\\)?-\\([0-9]+\\)?$" year)
          (setq from  (match-string 1 year)
                until (match-string 2 year)))
      (if (string-match "^\\([0-9]+\\)?$" year)
          (setq from  (match-string 1 year)
                until (match-string 1 year)))
      (if (/= 0 (length from))
          (push (format "year_from=%s" from) result))
      (if (/= 0 (length until))
          (push (format "year_to=%s" until) result))
      (setq query (mapconcat 'identity result "&"))
      (bib-cinii-query-buffer query)))

(provide 'bib-cinii)
