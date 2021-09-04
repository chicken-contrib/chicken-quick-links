(import (only (srfi 1) append-map remove take))
(import (only (srfi 13) string-every))
(import (srfi 69))
(import (chicken io) (chicken sort))
(import (http-client) (uri-common))
(import (html-parser) (sxml-transforms) (sxpath) (sxpath-lolevel))

(define (displayln x) (display x) (newline))

(define (string-blank? s) (string-every char-whitespace? s))

(define wiki-uri      "https://wiki.call-cc.org/")
(define gitweb-uri    "https://code.call-cc.org/cgi-bin/gitweb.cgi")
(define henrietta-uri "https://code.call-cc.org/cgi-bin/henrietta.cgi")

(define chicken-4-core
  '("srfi-1"
    "srfi-13"
    "srfi-14"
    "srfi-18"
    "srfi-69"))

(define (gitweb-repo-uri chicken-release egg-name egg-version)
  (parameterize ((form-urlencoded-separator "&"))
    (uri->string
     (update-uri
      (absolute-uri gitweb-uri)
      query: `(("p" . ,(string-append
                        "eggs-"
                        (number->string chicken-release)
                        "-latest.git"))
               ("a" . "tree")
               ("f" . ,(string-append egg-name "/" egg-version)))))))

(define (egg-wiki-uri chicken-release egg-name)
  (uri->string
   (update-uri
    (absolute-uri wiki-uri)
    path: `(/ "eggref" ,(number->string chicken-release) ,egg-name))))

(define (fetch-egg-version-numbers chicken-release egg-name)
  (let ((uri (parameterize ((form-urlencoded-separator "&"))
               (update-uri
                (absolute-uri henrietta-uri)
                query: `(("name" . ,egg-name)
                         ("release" . ,(number->string chicken-release))
                         ("mode" . "default")
                         ("listversions" . "1"))))))
    (remove string-blank? (with-input-from-request uri #f read-lines))))

(define all-tr-tags (sxpath '(// tr)))
(define all-td-tags (sxpath '(// td)))
(define all-a-tags (sxpath '(// a)))

(define (content-string node)
  (apply string-append (sxml:content node)))

(define (href-attribute node)
  (sxml:attr-from-list (sxml:attr-list node) 'href))

(define-record-type <egg>
  (make-egg name description license version-c4 version-c5)
  egg?
  (name        egg-name)
  (description egg-description)
  (license     egg-license)
  (version-c4  egg-version-c4 set-egg-version-c4!)
  (version-c5  egg-version-c5 set-egg-version-c5!))

(define eggs-by-name (make-hash-table))

(define (get-or-make-egg name description license)
  (or (hash-table-ref/default eggs-by-name name #f)
      (let ((egg (make-egg name description license #f #f)))
        (hash-table-set! eggs-by-name name egg)
        egg)))

(define (scrape-egg-from-tr-tag! chicken-release tr)
  (let ((tds (all-td-tags tr)))
    (and (= 6 (length tds))
         (let* ((a-tag (car (all-a-tags (car tds))))
                (name (content-string a-tag))
                (doc-url (href-attribute a-tag))
                (descrip (content-string (list-ref tds 1)))
                (license (content-string (list-ref tds 2)))
                (version (content-string (list-ref tds 5)))
                (egg (get-or-make-egg name descrip license)))
           (case chicken-release
             ((4) (set-egg-version-c4! egg version))
             ((5) (set-egg-version-c5! egg version)))
           egg))))

(define (scrape! chicken-release html-filename)
  (let ((sxml (with-input-from-file html-filename html->sxml)))
    (for-each (lambda (tr) (scrape-egg-from-tr-tag! chicken-release tr))
              (all-tr-tags sxml))))

(define (eggs)
  (let ((names (sort (hash-table-keys eggs-by-name) string-ci<?)))
    (map (lambda (name) (hash-table-ref eggs-by-name name))
         names)))

(scrape! 4 "index4.html")
(scrape! 5 "index5.html")

(define (chicken-and-egg->td chicken-release egg egg-version newest)
  (cond (egg-version
         `(td (@ (class ,(if (equal? egg-version newest) "new" "old")))
              (a (@ (href ,(egg-wiki-uri chicken-release (egg-name egg))))
                 "Doc")
              " "
              (a (@ (href ,(gitweb-repo-uri chicken-release
                                            (egg-name egg)
                                            egg-version)))
                 ,egg-version)))
        ((member (egg-name egg) chicken-4-core)
         `(td (@ (class "new"))
              "Core"))
        (else
         '(td (@ (class "old"))))))

(define (egg->tr egg)
  (let ((newest (or (egg-version-c5 egg)
                    (egg-version-c4 egg))))
    `(tr (td ,(egg-name egg))
         (td ,(egg-description egg))
         (td ,(egg-license egg))
         ,(chicken-and-egg->td 5 egg (egg-version-c5 egg) newest)
         ,(chicken-and-egg->td 4 egg (egg-version-c4 egg) newest))))

(define (main)
  (with-output-to-file "chicken-eggs.html"
    (lambda ()
      (SXML->HTML
       `(html
         (head
          (title "Chicken eggs")
          (style
              "html { font-family: sans-serif; }
              td, th, table { border: 1px solid black; }
              td, th { vertical-align: top; }
              .new { background-color: lightgreen; }
              .old { background-color: pink; }"))
         (body
          (h1 "Chicken eggs")
          (table
           (tr (th "Egg")
               (th "Description")
               (th "License")
               (th "C5")
               (th "C4"))
           ,@(map egg->tr (eggs)))))))))

(main)
