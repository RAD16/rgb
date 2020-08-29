;
; To load test file,
;   - in rgb.rkt, comment all ;; tests, then uncomment rgb.rkt #lang racket
;   - load rgb.rkt into buffer, then recomment #lang racket, save file.
;   - Then load test.rkt into repl. 
;    (this should work?)
;

#lang racket
(require racket/include)
(include "rgb.rkt")
(include "load.rkt")
(include "ui-rgb.rkt")


(define roster-101a
  (get-roster (hash-ref (car SEM) '101a)))


(print-attend (find-student-fn roster-101a
                               "Scary"))

(print-roster roster-101a)

(run-attendance-fn roster-101a
                     44    ;; date
                     '("Scary") ;; absent
                     '("Schlurpen" "Willer")) ;; late

(absent? '(20200901 absent))

(print-roster
 (remove*
  (append
   (find-student-fn* roster-101a '("Willer" "Rob"))
   (find-student-fn* roster-101a '("Dang Jesus" "Karen")))
  roster-101a))

