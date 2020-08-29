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

(course-roster (hash-ref (car SEM) '101a))

(print-attend (find-student-fn "Rob" roster-101a))

(student-absences (find-student-fn "Rob" roster-101a))

(map print-attend (find-student-fn*
  '("Rob"
    "Scary")
  roster-101a))

(map student-absences (find-student-fn*
      '("Rob"
        "Scary")
      roster-101a))

(student-absences (find-student-fn "Schlurpen" roster-101a))

(batch-push-session! '(2021 absent) roster-101a)

(push-attendance-fn roster-101a
                    2021
                    '("Karen")
                    '("Scary" "Schlurpen"))

(print-roster
 roster-101a)

(run-attendance-fn roster-101a
                     0012    ;; date
                     '("Dang Jesus") ;; absent
                     '("Scary")) ;; late
