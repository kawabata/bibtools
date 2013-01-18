= BibTools

  BibTools let you retrieve bibliographic data from ISBN or title, 
  and converts them to BibLaTeX format.
  
  It uses National Diet Library of Japan or Library of Congress in the U.S.
  
== init.el 設定

BibLaTeXのソートが正しく行われるよう、init.el に以下の設定を追加してください。
  
    (add-hook 'bibtex-mode-hook
              (lambda () (setq outline-regexp "[ \t]*title"
                               ;; fill はさせない。
                               fill-column 1000)))
    (let ((misc (assoc-default "Misc" bibtex-biblatex-entry-alist))
          (types '("artwork" "audio" "bibnote" "commentary" "image" "jurisdiction"
                  "legislation" "legal" "letter" "movie" "music" "performance" "review"
                  "softare" "standard" "video")))
      (dolist (type types)
        (add-to-list 'bibtex-biblatex-entry-alist
                     (cons type misc))))
    (bibtex-set-dialect 'biblatex)

== 実行例

    (bib-ndl-isbn "4150105928")
    "@book{岡部宏之_第二ファウンデーション,
      title = {第二ファウンデーション : 銀河帝国興亡史3},
      sorttitle = {ダイニ ファウンデーション},
      author = {アイザック・アシモフ},
      translator = {岡部宏之},
      sortname = {オカベ,ヒロユキ(1931-)},
      date = {1984.12},
      publisher = {早川書房},
      location = {東京},
      pagetotal = {366p},
      size = {16cm},
      price = {税込価格 : 420円},
      series = {ハヤカワ文庫. SF},
      ndc = {933},
      url = {http://api.porta.ndl.go.jp/ndlopac/cgi-bin/ndlopac/ndl-book?kywd=85026435},
      isbn = {4-15-010592-8},
      jpno = {85026435},
      ndlcn = {KS151-126},
      language = {JPN}
    }
    "
