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
(include "ui.rkt")


(pick-item
 (student-gradebook (car roster))
 assignment-title)

