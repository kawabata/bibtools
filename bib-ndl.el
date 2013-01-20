;;; bib-ndl.el --- NDL (National Diet Library of Japan) ISS access 

;; Copyright (C) 2013 Taichi Kawabata

;; Author: Taichi Kawabata <kawabata.taichi at gmail.com>
;; Keywords: isbn, bibtex, library

;;; Commentary
;; 
;; 国会図書館 ISS から様々な書誌情報を取り出し、BibLaTeXで出力する。
;;
;; 参考文献
;; - 国会図書館ダブリンコアメタデータ記述（DC-NDL）
;;   http://iss.ndl.go.jp/information/api/
;;   http://www.ndl.go.jp/jp/aboutus/standards/meta.html
;;
;; 検索式の記述方法については「国立国会図書館サーチ　外部提供インタフェース仕様書」を参照。

(require 'bib-xml)

(defvar bib-ndl-query-url
  "http://iss.ndl.go.jp/api/sru?operation=searchRetrieve&recordPacking=xml&recordSchema=dcndl&maximumRecords=20&onlyBib=true&query="
  "国会図書館ISS問い合わせURL。recordPacking=xml とすること。onlyBib=trueで高速化。")

;; NDLタイプのラベルと本の種類の対応表
;; cf. http://iss.ndl.go.jp/information/wp-content/uploads/2012/09/ndltype_ver.1.1_20120903.pdf
;; cf. BibLaTeX Manual 2.1 Entry Types
(defvar bib-ndl-types ; 
  '(
    ("Book" "図書" "book")
    ("Article" "記事・論文" "article")
    ("Journal" "雑誌" "periodical")
    ("Newspaper" "新聞" "newspaper")
    ("Sound" "録音資料" "audio")
    ("MovingImage" "映像資料" "video")
    ("ElectronicResource" "電子資料" "online")
    ("Map" "地図" "map")
    ("MusicScore" "楽譜" "music")
    ("StillImage" "静止画資料" "image")
    ("JapaneseClassicalBook" "和古書" "book")
    ("ChineseClassicalBook" "漢籍" "book")
    ("GovernmentPublication" "政府刊行物" "legislature")
    ("DoctoralDissertation" "博士論文" "thesis")
    ("ConferenceProceeding" "会議録" "proceedings")
    ("TechnicalReport" "規格・テクニカルリポート類" "report")
    ("MaterialsForVisuallyHandicappedPeople" "視覚障害者向け資料" "misc")
    ("MuseumMaterial" "博物資料" "misc")
    ("LegislativeInformation" "立法情報" "legislature")
    ("ReferenceInformation" "参考情報" "reference")
    ))

;; http://iss.ndl.go.jp/information/wp-content/uploads/2012/08/dcndl_rdf_format_ver.1.1_20111226.pdf
;; のうち、biblatexに対応しそうなものだけを選択。
(defvar bib-ndl-rdf 
  '(
    ;; 2-1
    (url     (rdfs:seeAlso ((rdf:resource . "http://id.ndl.go.jp/jpno/")) nil t))
    ;; 2-6
    (isbn    (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/ISBN"))))
    (issn    (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/ISSN"))))
    (isrn    (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/ISRN"))))
    (ismn    (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/ISMN"))))
    (pbno    (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/PBNO"))))
    (plno    (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/PLNO"))))
    (jpno    (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/JPNO"))))
    (usmarc  (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/USMARCNO"))))
    (ukmarc  (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/UKMARCNO"))))
    (brno    (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/BRNO"))))
    (sici    (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/DOI"))))
    (doi     (dcterms:identifier ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/SICI"))))
    ;; 2-8
    (title      dc:title rdf:Description rdf:value)
    (sorttitle  dc:title rdf:Description dcndl:transcription)
    ;(title1     dcterm:title)
    (subtitle   dcndl:alternative rdf:Description rdf:value) ; 少し意味は違う
    ;; 2-37
    (edition    dcndl:edition)
    ;; 2-42
    (authors    dcterms:creator)
    ;(sortname   dcterms:creator foaf:Agent dcndl:transcription)
    ;; 2-46
    (authors2   dc:creator)
    ;(author     (dc:creator nil "^\\(.+\\)[ 　／‖]著$"))
    (annotator  (dc:creator nil "^\\(.+\\)[ 　／‖]注釈$"))
    (editor     (dc:creator nil "^\\(.+\\)[ 　／‖]編$"))
    (editora    (dc:creator nil "^\\(.+\\)[ 　／‖]監修$"))
    (translator (dc:creator nil "^\\(.+\\)[ 　／‖]監?訳$"))
    ;; 2-51
    (publisher  dcterms:publisher foaf:Agent foaf:name)
    (location   dcterms:publisher foaf:Agent dcndl:location)
    (country    dcndl:publicationPlace)
    ;; 2-59
    (year       dcterms:date)
    (date       dcterms:issued)
    ;; 2-67
    (partinfo   dcndl:partInformation rdf:Description dcterms:title)
    ;; 2-75
    (abstract   dcterms:abstract)
    (note       dcterms:description)
    ;; 以下の値は属性値を取るので当面は使わない。
    (ndc        (dcterms:subject ((rdf:resource . "http://id.ndl.go.jp/class/ndc8")) nil t))
    (ndc        (dcterms:subject ((rdf:resource . "http://id.ndl.go.jp/class/ndc9")) nil t))
    (ndlc       (dcterms:subject ((rdf:resource . "http://id.ndl.go.jp/class/ndlc")) nil t))
    (call       dcndl:callNumber)
    (keywords   dcterms:subject rdf:Description rdf:value)
    (language   dcterms:language)
    (price      dcndl:price)
    (extent     dcterms:extent)
    ;; 2-84
    (origlanguage dcndl:originalLanguage)
    (material   (dcndl:materialType nil nil t))
    ;; 2-98
    (journaltitle dcndl:publicationName)
    ;; 2-102
    (volume     dcndl:publicationVolume)
    (part       dcndl:volume rdf:Description rdf:value)
    (number     dcndl:number)
    (issue      dcndl:issue)
    (pages      dcndl:pageRange)
    ;; 2-103
    (institution dcndl:degreeGrantor foaf:Agent foaf:name)
    ;; 2-113
    (copyright dcndl:rightsHolder) ;; 仕様書はdcterms:rightsHolder。
    ;; ??
    (series dcndl:seriesTitle rdf:Description rdf:value)
    ))

(defvar bib-ndl-notes
  '((origtitle . "原タイトル: ")
    (organization ."所属名: ")))

;;; functions

(defun bib-ndl-query (query)
  (bib-xml-query (concat bib-ndl-query-url query)))

(defun bib-ndl-get-rdfs (xml)
  "NDLのXMLデータから、recordDataのrdfのリストを取り出す。"
  ;; query は、recordPacking=xml とすること。
  (remove-if-not
   (lambda (x) (and (listp x) (equal (car x) 'rdf:RDF)))
   (bib-xml-get xml '(searchRetrieveResponse records record recordData))))

(defun bib-ndl-parse-rdf (rdf)
  "RDFデータから書誌情報を取り出し、ハッシュテーブルで返す。"
  (let* ((d (bib-xml-get rdf '(rdf:RDF dcndl:BibResource)))
         (table (make-hash-table))
         authors author translator)
    ;; `bib-ndl-rdf' にある指定を順番に処理。
    (loop for item in bib-ndl-rdf
          for key = (car item)
          for path = (cdr item)
          for val = (bib-xml-get d path)
          if val
          do (puthash key val table))
    (if bib-debug (message "table=%s" table))
    table))

(defun bib-ndl-process-bibdata (table)
  "TABLEの書誌情報を処理し、キー、タイプを計算してXMLを文字列に直す。"
  (let* ((authors    (gethash 'authors table))
         (authors2   (gethash 'authors2 table))
         (editor     (gethash 'editor table))
         (editora    (gethash 'editora table))
         (translator (gethash 'translator table))
         (publisher  (gethash 'publisher table))
         (annotator  (gethash 'annotator table))
         (title      (gethash 'title table))
         (date       (gethash 'date table))
         (year       (gethash 'year table))
         (notes      (gethash 'notes table))
         (country    (gethash 'country table))
         (language   (gethash 'language table))
         (url        (gethash 'url table))
         (material   (gethash 'material table))
         (ndc        (gethash 'ndc table))
         (ndlc       (gethash 'ndlc table))
         ;; material は、 ((rdf:resource
         ;; . http://purl.org/dc/dcmitype/MovingImage) (rdfs:label . 映
         ;; 像資料) (rdf:resource . http://ndl.go.jp/ndltype/DVD)
         ;; (rdfs:label . DVD)) のような形になる。
         (resource   (assoc-default 'rdf:resource material))
         (=type=     (or (and resource 
                              (string-match "http://.+/\\([^/]+\\)$" resource)
                              (cadr (assoc-default (match-string 1 resource) bib-ndl-types)))
                         "book"))
         (=key=)
         author sortname)
    ;;; authors/authors2 → author/sortname の処理（複雑なので注意）
    ;;
    ;; 【説明】
    ;; 国会図書館データでは、著者名を dc:creator と、dcterms:creatorで管理している。
    ;; これらは内容が重複するが、情報が異なる。
    ;; dcterms:creator (authors) は、読み・生年月日はあるが、役割はない。
    ;; dc:creator (authors2) は、読みはないが、役割が末尾にある場合が多い。
    ;; これらを整理して、authors から翻訳者などの情報を除去して、author/sortname を
    ;; 取り出す。
    ;; 
    ;; authors は、((foaf:Agent nil (foaf:name XX) (dcndl:transcription: yy)) ...) だが、
    ;; これを、(("漢字名" . "ふりがな") ...) のリストに変換する。
    (setq authors
          (let (name trans result)
            (dolist (author authors)
              (when (listp author)
                (setq name  (car (bib-xml-get (list author) '(foaf:Agent foaf:name)))
                      trans (car (bib-xml-get (list author) '(foaf:Agent dcndl:transcription))))
                (setq result (append result (list (cons name trans))))))
            result))
    ;; authors から、translator, editor, editora, annotator の正規形と前方一致するものを削除する。
    (let ((targets
           (mapcar (lambda (x) (replace-regexp-in-string "[-0-9 ,()‖　]+" "" x))
                   (append translator editor editora annotator))))
      (delete-if-not (lambda (x)
                       (when (car x)
                         (let ((checked (replace-regexp-in-string "[-0-9 ,()‖　]+" "" (car x))))
                           (loop for target in targets
                                 never (string-match checked target)))))
                     authors))
    ;; 残った authors から、author と sorttitle を分離する。
    (dolist (author-trans authors)
      (when (car author-trans)
        (setq author (append author (list (car author-trans)))))
      (when (cdr author-trans)
        (setq sortname (append sortname (list (cdr author-trans))))))
    ;; authorsが空ならば、authors2をauthorにする。
    (if (null authors) (setq author authors2))
    ;; author の "‖" を除去する。
    (setq author (mapcar (lambda (x) (replace-regexp-in-string "‖" ", " x)) author))
    ;; データベースを整理する。
    (remhash 'authors table)
    (remhash 'authors2 table)
    (puthash 'author author table)
    (if sortname (puthash 'sortname sortname table))
    ;; =key=
    (setq =key= (concat (bib-first-word (or (car author) (car publisher))) "_" (bib-first-word (car title))))
    ;; url
    (remhash 'url table)
    (setq url (assoc-default 'rdf:resource url))
    (if url (puthash 'url (list url) table))
    ;; date
    (remhash 'year table)
    (puthash 'date (or year date) table)
    ;; ndc/ndlc
    (if ndc
        (puthash 'ndc (list 
                       (replace-regexp-in-string
                        "^.+/\\([^/]+\\)$" "\\1"
                        (assoc-default 'rdf:resource ndc)
                        nil)) table))
    (if ndlc
        (puthash 'ndlc (list 
                        (replace-regexp-in-string
                         "^.+/\\([^/]+\\)$" "\\1" 
                         (assoc-default 'rdf:resource ndlc)
                         nil)) table))
    ;; language, countryが日本なら削除
    (if (equal language "jpn") (remhash 'language table))
    (if (equal country "JP") (remhash 'country table))
    ;; materials
    (remhash 'material table)
    (puthash '=type= (list =type=) table)
    (puthash '=key= (list =key=) table)
    table))

(defun bib-ndl (query)
  "Queryに従ったBibTeXデータのリストを返す。"
  (if bib-debug (message "debug: query=%s" query))
  (let* ((xml (bib-ndl-query query))
         (rdfs (bib-ndl-get-rdfs xml)))
    (if bib-debug (message "debug: xml=%s" xml))
    (if bib-debug (message "debug: rdfs=%s" rdfs))
    (mapcar (lambda (rdf)
              (bib-entry
               (bib-ndl-process-bibdata
                (bib-ndl-parse-rdf (list rdf)))))
            rdfs)))
  
;;; サンプルプログラム

(defun bib-ndl-jpno (jpno)
  (interactive "sJPNO=?")
  "JPNO番号からBibTeXエントリを生成する。"
    (bib-ndl (concat "(jpno=" jpno ")")))

(defun bib-ndl-isbn (isbn)
  (interactive "sISBN=?")
  "ISBN番号からBibTeXエントリを生成する。"
  ;(bib-ndl (concat "(isbn=" (bib-normalize-isbn isbn) ")")))
  (bib-ndl (concat "(isbn=" isbn ")")))

(defvar bib-ndl-mediatype
  '((1 . "book")
    (2 . "article")
    (3 . "newspaper")
    (4 . "kids")
    (5 . "references")
    (6 . "digital")
    (7 . "misc")
    (8 . "challenged")
    (9 . "legislature")))

(defun bib-ndl-bib-buffer (title &optional creator publisher year mediatype)
  (interactive 
   (list (read-string "Title (=XXX:exact, ^XXX:forward-match)? ")
         (read-string "Creator (=XXX:exact, ^XXX:forward-match)? ")
         (read-string "Publisher (=XXX:exact, ^XXX:forward-match)? ")
         (read-string "Year (e.g. 2012-, 1945-1960, etc.)? ")
         (let ((string
                (completing-read "Media Type ? " (mapcar 'cdr bib-ndl-mediatype))))
           (or (car (assoc (string-to-number string) bib-ndl-mediatype))
               (car (rassoc string bib-ndl-mediatype)))
           )))
  (flet ((query-check (name value)
           (cond ((null value) nil)
                 ((equal value "") nil)
                 ((= (string-to-char value) ?=) 
                  (concat name " exact " (substring value 1)))
                 ((= (string-to-char value) ?^)
                  (concat name "=^" (substring value 1)))
                 (t (concat name "=" value)))))
    (let* (result query from until
           (title (query-check "title" title))
           (creator (query-check "creator" creator))
           (publisher (query-check "publisher" publisher))
           (year (or year ""))
           (mediatype (if mediatype (format "mediatype=%s" mediatype))))
      (if title (push title result))
      (if creator (push creator result))
      (if publisher (push publisher result))
      (if (string-match "^\\([0-9]+\\)?-\\([0-9]+\\)?$" year)
          (setq from  (match-string 1 year)
                until (match-string 2 year)))
      (if (string-match "^\\([0-9]+\\)?$" year)
          (setq from  (match-string 1 year)
                until (match-string 1 year)))
      (if (/= 0 (length from))
          (push (format "from=%s" from) result))
      (if (/= 0 (length until))
          (push (format "until=%s-12-31" until) result))
      (if mediatype (push mediatype result))
      (setq query (concat "(" (mapconcat 'identity result ") AND (") ")"))
      (switch-to-buffer (get-buffer-create "*BibTeX*"))
      (bibtex-mode)
      (insert (mapconcat 'identity (bib-ndl query) "\n\n") "\n\n"))))

(provide 'bib-ndl)
