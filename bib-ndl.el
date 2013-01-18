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
    ("Journal" "雑誌" "journal")
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
    (title1     dcterm:title)
    (subtitle   dcndl:alternative rdf:Description rdf:value) ; 少し意味は違う
    ;; 2-37
    (edition    dcndl:edition)
    ;; 2-42
    (authors    dcterms:creator)
    ;(sortname   dcterms:creator foaf:Agent dcndl:transcription)
    ;; 2-46
    (author     (dc:creator nil "^\\(.+\\) 著$"))
    (annotator  (dc:creator nil "^\\(.+\\) 注釈$"))
    (editor     (dc:creator nil "^\\(.+\\) 編$"))
    (editora    (dc:creator nil "^\\(.+\\) 監修$"))
    (translator (dc:creator nil "^\\(.+\\) 監?訳$"))
    ;; 2-51
    (publisher  dcterms:publisher foaf:Agent foaf:name)
    (location   dcterms:publisher foaf:Agent dcndl:location)
    (country    dcndl:dcndl:publicationPlace)
    ;; 2-59
    (year       dcterms:date)
    (date       dcterms:issued)
    ;; 2-75
    (abstract   dcterms:abstract)
    (note       dcterms:description)
    ;; 以下の値は属性値を取るので当面は使わない。
    ;(ndc8       (dc:subject ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/NDC8"))))
    ;(ndc9       (dc:subject ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/NDC9"))))
    ;(ndlc       (dc:subject ((rdf:datatype . "http://ndl.go.jp/dcndl/terms/NDLC"))))
    (ndlcn      dcndl:callNumber)
    (keywords   dcterms:subject rdf:Description rdf:value)
    (language   dcterms:language)
    (price      dcndl:price)
    (extent     dcndl:extent)
    ;; 2-84
    (origlanguage dcndl:originalLanguage)
    (material   (dcndl:materialType nil nil t))
    ;; 2-98
    (journaltitle dcndl:publicationName)
    ;; 2-102
    (pages dcndl:pageRange)
    ;; 2-103
    (institution dcndl:degreeGrantor foaf:Agent foaf:name)
    ;; 2-113
    (copyright dcndl:rightsHolder) ;; 仕様書はdcterms:rightsHolder。
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
  "RDFデータから書誌情報を取り出し、attr-fieldsの形式で返す。"
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
    table))

(defun bib-ndl-process-bibdata (table)
  "TABLEの書誌情報を処理し、キー、タイプを計算してXMLを文字列に直す。"
  (let* ((authors    (gethash 'authors table))
         (author     (gethash 'author table))
         (translator (gethash 'translator table))
         (title      (gethash 'title table))
         (notes      (gethash 'notes table))
         (url        (gethash 'url table))
         (material   (gethash 'material table))
         ;; material は、 ((rdf:resource . http://purl.org/dc/dcmitype/MovingImage) (rdfs:label . 映像資料) (rdf:resource . http://ndl.go.jp/ndltype/DVD) (rdfs:label . DVD)) のような形になる。
         (resource   (assoc-default 'rdf:resource material))
         (=type=     (or (and resource 
                              (string-match "http://.+/\\([^/]+\\)$" resource)
                              (cadr (assoc-default (match-string 1 resource) bib-ndl-types)))
                         "book"))
         (=key=      (concat (bib-first-word (car author)) "_" (bib-first-word (car title)))))
    ;; 暫定処理
    (remhash 'url table)
    (setq url (assoc-default 'rdf:resource url))
    (if url (puthash 'url (list url) table))
    (remhash 'authors table)
    (when (null author) 
      (remhash 'author table)
      (setq author (puthash 'author (bib-xml-get authors '(foaf:Agent foaf:name)) table))
      (if author (puthash 'author author table)))
    (remhash 'material table)
    (remhash 'title1 table)
    (remhash 'year table)
    (puthash '=type= (list =type=) table)
    (puthash '=key= (list =key=) table)
    table))

(defun bib-ndl (query)
  "Queryに従ったBibTeXデータのリストを返す。"
  (let* ((xml (bib-ndl-query query))
         (rdfs (bib-ndl-get-rdfs xml)))
    (if bib-debug (message "debug: rdfs=%s" rdfs))
    (mapcar (lambda (rdf)
              (bib-entry
               (bib-ndl-process-bibdata
                (bib-ndl-parse-rdf (list rdf)))))
            rdfs)))
  
;;; サンプルプログラム

;;;###autoload
(defun bib-ndl-jpno (jpno)
  "JPNO番号からBibTeXエントリを生成する。"
  (bib-ndl (concat "(jpno=" jpno ")")))

;;;###autoload
(defun bib-ndl-isbn (isbn)
  "ISBN番号からBibTeXエントリを生成する。"
  (save-match-data ;; for interactive use
    (bib-ndl (bib-xml-query 
              (concat "(isbn=" (bib-normalize-isbn isbn) ")")))))

(provide 'bib-ndl)
