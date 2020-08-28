;;; Procedures for serializing data to disk

(require racket/serialize)

;; Database file
(define DBFILE "rgb.db")

;; variable: options for (open-output-file)
;; set to 'replace while testing to avoid hassle of recreating file every tim
(define output-opt
  'replace
  ;; 'update
  ;; 'can-update ;; open existing file w/out truncating, create if doesn't exist
  ;; 'append ;; append to end of file whether exists or not
  )

;; rgb-write!
;; (data) -> (void)
;; serialize and write data to file
;; intended to consume SEM list and write it to fil
(define rgb-write!
  (lambda (db)
    (let ([out (open-output-file DBFILE #:exists output-opt)])
      (write (serialize db) out)
      (close-output-port out))))

;; rgb-read
;; (serialized data from file) -> deserialized program data
;; read from file and deserialize data
(define rgb-read
  (lambda (dbfile)
    (let* ([in (open-input-file dbfile)]
          [data (deserialize (read in))])
      (close-input-port in)
      data)))

;; rgb-load
;; sets SEM as the result of rgb-read
(define rgb-load!
  (lambda()
    (set! SEM (rgb-read DBFILE))))
