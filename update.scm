(import (only (srfi 1) append-map remove take))
(import (only (srfi 13) string-every))
(import (srfi 69))
(import (chicken io) (chicken sort))
(import (http-client) (uri-common))
(import (html-parser) (sxml-transforms) (sxpath) (sxpath-lolevel))

(define (displayln x) (display x) (newline))

(define (string-blank? s) (string-every char-whitespace? s))

(define-record-type <egg-version>
  (make-egg-version number)
  egg-version?
  (number egg-version-number)
  (c5-doc-url egg-version-c5-doc-url set-egg-version-c5-doc-url!)
  (c5-git-url egg-version-c5-git-url set-egg-version-c5-git-url!)
  (c4-doc-url egg-version-c4-doc-url set-egg-version-c4-doc-url!)
  (c4-git-url egg-version-c4-git-url set-egg-version-c4-git-url!))

(define-record-type <egg>
  (make-egg name description license versions-by-number)
  egg?
  (name egg-name)
  (description egg-description)
  (license egg-license)
  (versions-by-number egg-versions-by-number))

(define base-uri "https://code.call-cc.org/cgi-bin/henrietta.cgi")

(define (fetch-egg-version-numbers egg-name chicken-release)
  (let ((uri (parameterize ((form-urlencoded-separator "&"))
               (update-uri
                (absolute-uri base-uri)
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

(define eggs-by-name (make-hash-table))

(define (get-or-make-egg name description license)
  (let ((egg (hash-table-ref/default eggs-by-name name #f)))
    (or egg (let ((egg (make-egg name description license (make-hash-table))))
              (hash-table-set! eggs-by-name name egg)
              egg))))

(define (get-or-make-version egg number)
  (let ((version (hash-table-ref/default (egg-versions-by-number egg)
                                         number #f)))
    (or version (let ((version (make-egg-version number)))
                  (hash-table-set! (egg-versions-by-number egg)
                                   number
                                   version)
                  version))))

(define (scrape-egg-from-tr-tag! chicken-release tr)
  (let ((tds (all-td-tags tr)))
    (and (= 6 (length tds))
         (let* ((a-tag (car (all-a-tags (car tds))))
                (name (content-string a-tag))
                (doc-url (href-attribute a-tag))
                (descrip (content-string (list-ref tds 1)))
                (license (content-string (list-ref tds 2)))
                (number (content-string (list-ref tds 5)))
                (egg (get-or-make-egg name descrip license))
                (version (get-or-make-version egg number)))
           (case chicken-release
             ((5)
              (set-egg-version-c5-doc-url! version doc-url)
              ;; (set-egg-version-c5-git-url! version ...)
              )
             ((4)
              (set-egg-version-c4-doc-url! version doc-url)
              ;; (set-egg-version-c4-git-url! version ...)
              ))
           egg))))

(define (scrape! chicken-release html-filename)
  (let ((sxml (with-input-from-file html-filename html->sxml)))
    (for-each (lambda (tr) (scrape-egg-from-tr-tag! chicken-release tr))
              (all-tr-tags sxml))))

(define (scrape-all-versions-of-egg! egg chicken-release)
  (for-each (lambda (number) (get-or-make-version egg number))
            (fetch-egg-version-numbers (egg-name egg) chicken-release)))

(define (eggs)
  (let ((names (sort (hash-table-keys eggs-by-name) string-ci<?)))
    (map (lambda (name) (hash-table-ref eggs-by-name name))
         names)))

(define (egg-versions egg)
  (let ((numbers (sort (hash-table-keys (egg-versions-by-number egg))
                       string-ci>?)))
    (map (lambda (number)
           (hash-table-ref (egg-versions-by-number egg) number))
         numbers)))

(scrape! 4 "index4.html")
(scrape! 5 "index5.html")

(for-each (lambda (egg)
            (scrape-all-versions-of-egg! egg 4)
            (scrape-all-versions-of-egg! egg 5))
          (take (eggs) 10))

(define (maybe-link link-text url)
  (if (and url (not (eq? url (void))))
      `((a (@ (href ,url)) ,link-text))
      '()))

(define (egg-and-version->tr egg version first-version?)
  `(tr ,@(if first-version?
             `((td ,(egg-name egg))
               (td ,(egg-description egg))
               (td ,(egg-license egg)))
             `((td)
               (td)
               (td)))
       (td ,(egg-version-number version))
       (td ,@(maybe-link "Doc" (egg-version-c5-doc-url version)))
       (td "")
       (td ,@(maybe-link "Doc" (egg-version-c4-doc-url version)))
       (td "")))

(with-output-to-file "chicken-eggs.html"
  (lambda ()
    (displayln
     (SXML->HTML
      `(html
        (head
         (title "Chicken eggs")
         (style
             "td, th, table { border: 1px solid black; }
              td, th { vertical-align: top; }"))
        (body
         (h1 "Chicken eggs")
         (table
          (tr (th "Egg")
              (th "Description")
              (th "License")
              (th "Version")
              (th "C5")
              (th "C5")
              (th "C4")
              (th "C4"))
          ,@(append-map
             (lambda (egg)
               (cons (let ((version (car (egg-versions egg))))
                       (egg-and-version->tr egg version #t))
                     (map (lambda (version)
                            (egg-and-version->tr egg version #f))
                          (cdr (egg-versions egg)))))
             (eggs)))))))))
