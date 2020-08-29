;; User interface for RAD Gradebook

#lang racket
(require racket/include)
(include "rgb.rkt")

#|
      Menu:         Description
      ------------------------------------------------
      Main          display courses for current semester
      Course        list of students
      Student       student info, including gradebook and attendance
      Semester      list of semesters

      All menus should allow
         making selections
         calling functions to
            act on visible data
            change menus
            save/quit, etc.
|#


;; selector: (list) . proc -> (list item selection)
;; consumes a list and a proc, print a numbered list of those list items,
;; asks user to enter a number for a selection
;; proc is the struct accessor procedure appropriate to the data
;; ex. (selector roster student-name) -> list of student names on roster
;; ex. (selector gradebook assignment-title) -> list of assignment names
(define selector
 (lambda (lst proc)
  (letrec ([n 1]
          [iter (lambda (n lst)
                  (cond [(null? lst) empty]
                        [else (printf "~a. ~a.\n" n (proc (car lst))) 
                              (iter (add1 n) (cdr lst))]))])
    (iter n lst)
    (printf "Selection: \n")
    (display (proc (list-ref lst (sub1 (read))))))))

;; test
;(define test-roster (course-roster (hash-ref (car SEM) '101a)))
;(selector test-roster student-name)

#;(define select-student
	(lambda (lst)
		(letrec ([n 1]
             [iter (lambda (n lst)
                     (cond [(null? lst) empty]
                           [else (printf "~a. ~a.\n" n (student-name (car lst))) 
                                 (iter (add1 n) (cdr lst))]))]) 
      (iter n lst))
    (printf "Selection: \n")
    (display (student-name (list-ref lst (sub1 (read)))))))

;; test
;; (select-student (course-roster (hash-ref (car SEM) '101a)))


;; pick-item: (listof menu items . selector) -> item
;; offers user a list of students, user enters number,
;; function returns the selected item.
;; "seletor" is the accessor necessary for printf to print
;; the item.
;; ex. (pick-item roster student-first) -> display of student's
;;        first names.
;; ex. (pick-item student-gradebook assignment-title) -> display of assignments
;;        in a student's gradebook.
(define pick-item
	(lambda (lst selector)
		(let* ([n 1])
      (for-each (lambda (n lst)
                  (printf "~a. ~a\n" n
                          (selector lst)))
                (map add1 (build-list (length lst) values))
                lst))
    (printf "Selection: \n")
    (list-ref lst (sub1 (read)))))


;; menu-main: semester hash -> void.
;; user-menu displays list of courses
;; in the most recent semester
;; options currently: pick a course
;; options wishlist: to-semester-menu, new-course
(define menu-main
  (lambda (sem)
    (cond [(null? sem) (display "Empty database")]
          [else
           (display "------* Main Menu *------")
           (newline)
           (menu-course
            (pick-item (hash-values sem)
                       (lambda (x) (course-title x))))])))

(define menu-main.v4
  (lambda (sem)
    (cond [(null? sem) (display "Empty database")]
          [else
           (display "------* Main Menu *------")
           (newline)
           (let ([input (read)])
             (case input))
           (menu-course
            (pick-item (hash-values sem)
                       (lambda (x) (course-title x))))])))



(define menu-main.v2
  (lambda (sem)
    (cond [(null? sem) (display "Empty database")]
          [else
           (display "------* Main Menu *------")
           (newline)
           (case (pick-item (hash-keys sem)
                            (lambda (x) hash-ref sem x))
             ['alp "zoom zoom ALP!!!!"]
             ['101a "one oh one OOOOHHHHHH!!!"]
             ['aclt "AckaTack!!!!"]
             [else "wooooops...."])])))

(define menu-main.v1
  (lambda (sem)
    (cond [(null? sem) (display "Empty database")]
          [else
           (letrec ([fun (lambda ()
                           (newline)
                           (display "------* Main Menu *------")
                           (pick-item (hash-keys sem)
                                      (lambda (x) hash-ref sem x))
                           (fun))])
             (fun))])))

;; menu-course: course struct -> void.
;; user-menu displays course roster
;; options currently: pick a student
;; options wishlist: to-main-menu, new-student
(define menu-course
  (lambda (course)
    (cond [(null? course) (display "Not a course, you poop.")]
          [else
            (display "------* Course Menu *------")
            (newline)
            (print-course-info course)
            (menu-student
             (pick-item (course-roster course) student-first))])))


;; menu-student: student struct -> void.
;; user-menu displays student info
;; options currently: nothing
;; options wishlist:
;;     to-course-menu, to-main-menu, to-gradebook, to-attendance,
;;     edit-student-info
(define menu-student
  (lambda (student)
    (cond [(null? student) (display "Not a course, you poop.")]
          [else
           (display "------* Student Menu *------")
           (newline)
           (print-student student)])))


;; test

#|
(menu-main.v3 (car SEM))

(hash-keys (car SEM))



(menu-course (hash-ref (car SEM) '101a))
(hash-keys (car SEM))
|#
