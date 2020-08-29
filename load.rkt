#lang racket
(require racket/include)
(include "rgb.rkt")
;(include "ui.rkt")

;; test courses
(define dcourse1 (list "English 101" "f2020" 12342 "CMA" '()))
(define dcourse2 (list "ALP 053" "f2020" 52342 "CMA" '()))
(define dcourse3 (list "ACLT 053" "f2020" 82342 "CMA" '()))

;; test semesters
(set! SEM (cons (hash 'F2020 dcourse1) SEM))

;;; Test cases 

(define new-Rob (student "P-Penis" "Rob" "new-email@penis" "English 101" 3))

(student-absences new-Rob)
(set-absences-count! new-Rob)


(add-session! '(20200906 absent) new-Rob)

(print-attend-report new-Rob)
(count-absences Karen)
(set-student-absences! new-Rob 0)


(define Rob (student  "Penis" "Rob" "email@penis" "English 101" 3)) 
(define Karen (student  "Vag" "Karen" "email@vag" "English 101" 2)) 
(define Floppy (student  "Pooper" "Floppy" "fpoops@floppy" "English 101" 34)) 
(define Scary (student  "Fyork" "Scary" "fscary@fyok" "English 101" 49)) 
(define Jebus (student  "Monk" "Dang Jesus" "mjesus@dang" "English 101" 88)) 
(define Schlurp (student  "Frenk" "Schlurpen" "fschlurp@frenk" "English 101" 39)) 
(define Big-Gulp (student  "Gully" "Willer" "willer@gully" "English 101" 21)) 

(define roster (list Rob Karen Floppy Scary Jebus Schlurp Big-Gulp))
roster

(define Essay-1 (assignment "Politics of Education" 20200919 95 #f))
(define Essay-2 (assignment "Research Project" 20201215 75 #f))
(define Essay-3 (assignment "Through the Ass Swiftly" 20201215 75 #f))


;;; TESTS
(map create-semester! '(f2019 s2020 f2020))

(create-course! (car SEM) '101a (list "English 101" "fall 2020" 123129 "CSM" roster))
(create-course! (car SEM) 'aclt dcourse3)
(create-course! (car SEM) 'alp dcourse2)

(map (lambda (x y) (create-course! (car SEM) x y))
     '(101a aclt alp)
     '((list "English 101" "fall 2020" 123129 "CSM" roster) dcourse2 dcourse3))


(map (lambda (x) (add-assignment! x Karen)) '(Essay-3 Essay-1 Essay-2))
(map (lambda (x) (add-assignment! x Rob)) '(Essay-1 Essay-2 Essay-3))

(define monday '(20200901 present))
(define tuesday '(20200902 present))
(define wednesday '(20200903 present))
(define thursday '(20200904 present))
(define friday '(20200905 present))

(define week (list monday tuesday wednesday thursday friday))
(print-roster
 (course-roster (hash-ref (car SEM) '101a)))
(set! roster (course-roster (hash-ref (car SEM) '101a)))
