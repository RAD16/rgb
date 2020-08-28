;#lang racket

(require racket/serialize)
(require racket/include)
(include "io-rgb.rkt")

;;; RGB: the RAD GradeBook

#|
	DATA STRUCTURES as STRUCTS and LISTS
      Semesters (list): list of semesters
      Semester (hash) hash table of courses
      Course (struct): title semester CRN section roster
      Roster (list): student-1 student-2 student-3 ... 
      Student (struct): name email course id gradebook attendance
      Gradebook (list): assignment-1 assignment-2 assignment-3 ... 
      Assignment (struct): title deadline grade late?
      Attendance (list): session-1 session-2 session-3 ... 
		  Session    (pair): session-date attend? 
|#

;;;;;;;; DATA ;;;;;;;;;;

;; Semesters list
;; SEM
;; Ex. (list S2020 F2019 M2019 S2019 W2019...)
(define SEM '())

;; Course: struct of course info
(serializable-struct course
	(title
	 semester
	 CRN
	 section
	 [roster #:mutable]))

;; Student: struct of student info
(serializable-struct student
	(last
   first
	 email 
	 course 
   id 
	 [gradebook #:auto #:mutable] 
	 [attendance #:auto #:mutable]) #:auto-value '())

;; Assignment: struct of assignment info
(serializable-struct assignment 
  (title
  [deadline #:mutable]
  [score #:mutable]
  late))


;;;;;;;;;; Functions on data ;;;;;;;;;;;;;

;; create-semester!
;; title symbol -> hash added to SEM list of semesters
;; update master SEM list with the current semester
;; prior to creating courses
(define create-semester!
  (lambda (sem)
    (define sem (make-hash))
    (set! SEM (cons sem SEM))))

;; user-course-info: void -> listof user inputs
;; calls (make-prompt) with field strings
;; returns the list of user inputs corresponding to fields 
;; ex. to prompt user to input course info
;;     (create-!course 'E101 (car SEM) (user-course-info))
;; ex. to enter course info manually:
;;     (create-course! 'sex101 (car SEM) (list "Sex 101" "sex 2020" 123123 "CMA" 'none))
(define user-course-info
	(lambda ()
    (make-prompt
     "Course title: "
     "Semester: "
     "CRN: "
     "Section: "
     "Creating Roster... ")))

;; create-course!
;; title symbol . semester hash . list of strings -> void
;; consumes title symbol for course, semester hash, and a list
;; of info strings. 
;; creates a course from user input or manually, adds to courses hash 
;; ex. to prompt user to input course info
;;     (create-course! 'E101 (car SEM) (user-course-info))
;; ex. to enter course info manually:
;;     (create-course! 'eng101 (car SEM) (list "Engl 101" "fall 2020" 123123 "CMA" 'none))
(define create-course!
	(lambda (sem title info)
		(hash-set! sem title
                   (course
                   (list-ref info 0) ; title
                   (list-ref info 1) ; semester
                   (list-ref info 2) ; CRN
                   (list-ref info 3) ; section 
                   (list-ref info 4))))) ; roster

(define create-course!.v2
	(lambda (sem title info)
    (cond [(null? info) (newline)]
          [else (hash-set! sem title
                           (course (car info)))
                (create-course!.v2 title sem (cdr info))])))

;; get-courses: semester hash -> (courses (as hash values))
;; Consumes a semester hash and returns the keys (course titles)
(define get-courses
	(lambda (table)
    (cond [(null? table) display "Empty: No courses to list"]
          [else (hash-values table)])))

;; print-courses: semester hash -> (void)
;; Consumes a semester hash and displays the keys (course titles)
(define print-courses
	(lambda (table)
    (cond [(null? table) display "Empty: No courses to list"]
          [else (map course-title (get-courses table))])))


;; print-course-info: (semester hash . course title) -> (void)
;; prints course information
;; consumes semester hash and title symbol of course
(define print-course-info
	(lambda (course)
		(printf "Course title: ~a\n" 
            (course-title course))
		(printf "Semester: ~a\n"     
            (course-semester course))
		(printf "CRN: ~a\n"          
            (course-CRN course))
		(printf "Section: ~a\n"      
            (course-section course))))

;;;;;;;;
;; Do I NEED THIS ONE? 
;; create-roster: roster name string -> list of students
;; calls 'get-names' to create a roster from user input
;; REFACT: have user-input be a separate function that takes strings for prompting, have THIS function take a list of student structs and add them to the hash table SDB

;-------------- Progress ----------------
#;(define create-roster
	(lambda (roster-title)
		(letrec ([names '()]
             [get-names 
                (lambda (input names) 
                  (display "Name: ")
                (cond ((eq? input 'q) names)
                    (else 
                      (get-names (read) (cons input names)))))])
        (display "Name: ")
				(make-std (get-names (read) names)))))

;; New Roster by looping make-std
#;(define create-roster
	(lambda () 
		(newline)
		(display "Create Student? (y) ")
		(cond [(not (eq? 'y (read))) '()]
          [else	(cons (make-std (user-std-info)) (create-roster))])))

;; get-roster: (semester hash . course name) -> (listof students)
;; consumes semester hash and the name of the course,
;; returns the roster (listof student structs)
;; useful to pass to (print-roster)
(define get-roster
	(lambda (course)
    (course-roster course)))

;; print-roster: (semester hash . course name) -> void
;; consumes a semester and course title and prints
;; the names of students on the course roster
(define print-roster
	(lambda (roster)
    (cond ((null? roster) (display "----") (newline))
          (else (printf "~a, ~a\n"
                    (student-last (car roster))
                    (student-first (car roster)))
                (print-roster (cdr roster))))))

;; user-std-info: (void) -> string of student info
;; Read user input to be added to student struct
(define user-std-info 
	(lambda ()
    (make-prompt
     "Data Label: "
     "Last name: "
     "First name: "
     "Email: "
     "Course: "
     "Student ID: ")))

;; create-student:
;; (listof info strings) -> student struct
;; Make Student struct from user input
(define create-student!
	(lambda (std-info)
    (student
     (list-ref std-info 0) ; label
     (list-ref std-info 1) ; last name
     (list-ref std-info 2) ; first name
     (list-ref std-info 3) ; email
     (list-ref std-info 4) ; course
     (list-ref std-info 5)))) ; student id number

(define create-student.v3
	(lambda (std-info)
    (student
     (map values (map (curry list-ref std-info) '(0 1 2 3 4 5))))))

(define create-student.v2
	(lambda (std-info)
    (student
     (values
      (map (curry list-ref std-info) '(0 1 2 3 4 5))))))

;; add-student!:
;; (course title . student label) -> (void)
;; consumes a course struct, student struct, and adds the student
;; onto the roster list of the course.
(define add-student!
	(lambda (course student)
		(set-course-roster! course
                        (cons student (course-roster course)))))

;; print-student: (student struct) -> (void)
;; Consumes student struct and displays student info
(define print-student
	(lambda (student)
    (cond [(void? student) (display "Not a student")]
          [else
            (printf "---- Student Profile -----\n")
            (printf "Name: ~a ~a\n"    (student-first student) (student-last student)) 
            (printf "Email: ~a\n"      (student-email student))
            (printf "Course: ~a\n"     (student-course student))
            (printf "Student ID: ~a\n" (student-id student))
            (printf "> Gradebook\n")
            (printf "> Attendance\n")
            (printf "--------------------------\n")])))


;; find-student-ln: ((listof student) . string) -> (student?)
;; Find Student by Last Name
;; search for student in roster by last name.
;; Consumes a roster and a student's last name as a string,
;; returns a matching student struct.
;; ex. print-student find-student roster "Rob" -> Rob<#student>
(define find-student-ln
  (lambda (roster student)
    (or (findf (lambda (x)
                 (equal? student (student-last x))) roster)
        (printf "~a not a last name in roster.\n" student))))

;; find-student-fn: ((listof student) . string) -> (student?)
;; Find Student by First Name
;; search for student in roster by first name.
;; Consumes a roster and a student's first name as a string,
;; returns a matching student struct.
;; ex. (print-student (find-student roster "Rob")) -> printed info about Rob 
(define find-student-fn
  (lambda (roster student)
    (or (findf (lambda (x)
                 (equal? student (student-first x))) roster)
        (printf "~a not a first name in roster.\n" student))))

;; find-student-id: ((listof student) . string) -> (student?)
;; Find Student by ID number
;; search for student in roster by student ID number.
;; Consumes a roster and a student's ID number as an exact number.
;; returns a matching student struct.
;; ex. (print-student (find-student roster 34)) -> printed info about Floppy 
(define find-student-id
  (lambda (roster student)
    (or (findf (lambda (x)
                 (equal? student (student-id x))) roster)
        (printf "~a not a student ID number in roster.\n" student))))

;; get-gradebook: (student) -> (listof assignments)
;; used by (print-gradebook)
(define get-gradebook 
	(lambda (student)
    (student-gradebook student)))

;; consumes a gradebook (listof assignments) and prints the list of assignments in gradebook
(define print-gradebook 
	(lambda (student)
    (cond [(null? student) (display "---")]
          [else
           (map (lambda (x)
                  (printf "~a\n" (assignment-title x)))
                (get-gradebook student))])))


(define user-assignment-info
	(lambda ()
    (make-prompt
     "Assignment title: "
     "Deadline (e.g. 20200901): "
     "Score: "
     "Late: ")))

(define create-assignment!
	(lambda (assign-info)
    (assignment
     (list-ref assign-info 0) ; title
     (list-ref assign-info 1) ; deadline
     (list-ref assign-info 2) ; score
     (list-ref assign-info 3)))) ; late?

;; print-assignment: (assignment struct) -> prints detailed struct info
;; consumes an assignment, displays its details. 
(define print-assignment
  (lambda (assignment)
    (cond [(void? assignment) (display "Not an assignment")]
          [else
           (display "---- Assignment Detail -----\n")
           (printf "Title: ~a\n" (assignment-title assignment)) 
           (printf "Deadline: ~a\n" (assignment-deadline assignment)) 
           (printf "Score: ~a\n"    (assignment-score assignment))
           (printf "Late?: ~a\n"    (assignment-late assignment))
           (printf "--------------------------\n")])))


;; find-assignment: (listof assignment) -> assignment struct
;; find assignment by title
(define find-assignment
  (lambda (gradebook assignment)
    (or (findf (lambda (x)
                 (equal? assignment (assignment-title x))) gradebook)
        (printf "~a not an assignment in gradebook.\n" assignment))))


;; add-assignment!: (student struct . assignment struct) -> (void)
;; consumes student and assignments structs, sets the student gradebook
;; with the new list of assignments (gradebook)
(define add-assignment!
	(lambda (student assignment)
	(set-student-gradebook! student 
		(cons assignment (student-gradebook student))))) 

;; create-session: (session int . status sym) -> (pair)
;; consumes an int and a attend status, produces a pair with a date - attend? pair
;; ex. (20200820 'present)
;; ex. (20200820 'absent)
;; ex. (20200820 'late)
(define create-session
 (lambda (session stat)
   (list session stat)))

;; add-session!: (student . session) -> (void)
;; Add session pair to student attendance sheet
;; consumes a student and a session pair, adds session onto
;; student's attendance sheet
(define add-session!
	(lambda (student session)
    (set-student-attendance! student 
      (cons session (student-attendance student))))) 

;; batch-add-session!: (session . roster) -> (void)
;; consumes a session pair and a roster list,
;; adds a session to every student in roster
(define batch-add-session!
  (lambda (roster session)
    (map (lambda (x) (add-session! x session)) roster)))

;; print-attend: (student) -> (void)
;; consumes a student struct, prints attendance info
;; as (session . status) pairs
(define print-attend
  (lambda (student)
    (for-each (lambda (x) (printf "~a\n" x)) (student-attendance student))))

;; print-attend.v2: (student struct) -> (void) <== Current implementation
;; uses map and currying to be more succinct
(define print-attend.v2
  (lambda (student)
    (map (lambda (x) (printf "~a\n" x)) (student-attendance student))))

(define print-attend.v1
  (lambda (student)
    (letrec ([attend (student-attendance student)]
             [fun (lambda (attend)
                     (cond [(null? attend) (printf "-----\n")]
                           [else (display (car attend))
                                 (newline)
                                 (fun (cdr attend))]))])
      (fun attend))))

;; print-attend-report: (student . option) -> (void)
;; *** ??? DEPRECATE *** in favor of using (sub-roster).
;; consumes student and an option, prints sessions student
;; was listed as "option" ('present 'late 'absent).
;; (lambda args) makes it variadic, so if no option is provided
;; it prints a full attendance report.
;; print-attend-report.v2
(define print-attend-report
  (lambda args
    (cond [(< 2 (length args)) (display "Too many arguments. Expect: (student? option)")]
          [(not (student? (car args))) (display "Expected student struct.")]
          [else (cond [(null? (cdr args)) (print-attend (car args))]
                      [(not (or (eq? (cadr args) 'present)
                                (eq? (cadr args) 'absent)
                                (eq? (cadr args) 'late)))
                       (display "Bad option! Expected no option, or 'present, 'absent, 'late")]
                      [else (map (lambda (x)
                                   (cond [(eq? (cadr args) (cadr x))
                                          (printf "~a\n" x)]))
                                 (student-attendance (car args)))])])))

;; print-attend-report.v1
(define print-attend-report.v1
  (lambda args
    (cond [(< 2 (length args)) (display "Too many arguments. Expect: (student? option)")]
          [(not (student? (car args))) (display "Expected student struct.")]
          [else (cond [(null? (cdr args)) (print-attend (car args))]
                      [(not (or (eq? (cadr args) 'present)
                                (eq? (cadr args) 'absent)
                                (eq? (cadr args) 'late)))
                      (display "Bad option! Expected no option, or 'present, 'absent, 'late")]
                      [else
                        (letrec ([attend (student-attendance (car args))]
                                 [fun (lambda (attend)
                                        (cond [(null? attend) (display "---")]
                                              ;; else, if option matches the second field
                                              ;; in the attendance pair e.g. (20200819 'absent)
                                              [else (cond [(eq? (cadr args) (cadar attend))
                                                            (display (car attend))
                                                            (newline)
                                                            (fun (cdr attend))]
                                                          [else (fun (cdr attend))])]))])
                          (fun attend))])])))


;; run-attendance.v3: (roster day noshows tardy) -> batch-runs attendance
;; consume roster, day then aleady-parsed sub-rosters
(define run-attendance
  (lambda (roster day noshows tardy)
    ;; line below combines noshows and tardy, then produces a roster
    ;; with just those students, then removes those students from the master roster
    ;; all to create the student group "inclass".
    ;; really hacky, but it's stable for now
    (let* ([inclass (remove* (sub-roster roster
                                         (append noshows tardy))
                             roster)]
           [states (list `(,day absent) `(,day late) `(,day present))]
           [groups (list
                    (sub-roster roster noshows)
                    (sub-roster roster tardy)
                    inclass)])
      (map batch-add-session! groups states))))

;; runa-ttendance.v3 <== Current implementation
(define run-attendance.v3 
  (lambda (roster day noshows tardy)
    (let* ([inclass (remove* (append noshows tardy) roster)]
           [states (list `(,day absent) `(,day late) `(,day inclass))]
           [groups (list noshows tardy inclass)])
      (map batch-add-session! groups states))))

;; run-attendance: (roster day no-shows)
;; consumes a roster, day (e.g. 20200812) list of no-shows,
;; adds the appropriate session to each student's attendance.

(define run-attendance.v1
  (lambda (roster day no-shows)
    (letrec ([n 1]
             [fun (lambda (roster n)
                    (cond [(null? roster)]
                          [else (cond [(member n no-shows)
                                       (add-session! (car roster) `(,day absent))]
                                      [else
                                        (add-session! (car roster) `(,day present)) ])
                              (fun (cdr roster) (+ 1 n))]))])
          (fun roster n))))

;;;; 
(define run-attendance.v2
  (lambda (roster day noshow)
    (batch-add-session! (car (sort-attend noshow roster)) `(,day absent))
    (batch-add-session! (cadr (sort-attend noshow roster)) `(,day present))))


;; pick-student: (listof student) -> student
;; offers user a list of students, user enters number,
;; function returns the selected student.
(define pick-student
	(lambda (roster)
		(letrec ([n 1]
             [fun (lambda (n roster)
                (cond [(null? roster) empty]
                      [else (printf "~a. ~a ~a.\n" n
                                    (student-first (car roster)))
                                    (student-last (car roster))
                        (fun (add1 n) (cdr roster))]))]) 
            (fun n roster))
    (printf "Selection: \n")
    (display (student-first (list-ref roster (sub1 (read)))))))

; make-prompt: (list of Strings) -> (listof any)
; Consumes list of menu fields (strings)
; which it presents to user, asking for input 
; produces an ordered list of user inputs
(define make-prompt
  (lambda menu
		(letrec ([lst '()]
             [fun (λ (menu lst)
                  (cond [(null? menu) lst]
                        [else (display (car menu))
                              (fun (cdr menu)
                              (cons (read) lst))]))])
      (reverse (fun menu lst)))))

;; sort-attend: (roster . (listof ints)) -> (listof picks) (listof others)
;; consume roster and list of numbers,
;; return two lists: (listof (students who match "picks")).
;;                   and (listof (students who do NOT match "picks"))
;; (listof ints) is the item number, NOT the index.
;; ex. (sort-attend '('Jamal 'Lashawn 'Ibrahim) '(1 3)) -> '('Jamal 'Ibrahim)
;;                                                         '(Lashawn)
(define sort-attend
  (lambda (lst picks)
    (cond [(null? picks) '()]
          [(eq? 0 (car picks)) lst]
          [else
           (let ([picked (map (curry list-ref lst) (map sub1 picks))])
             (values picked (remove* picked lst)))])))

;; sort-attend.v2.1 <== Current implementation
(define sort-attend.v2.1
  (lambda (lst picks)
    (cond [(null? picks) '()]
          [(eq? 0 (car picks)) lst]
          [else
           (let ([picked (map (curry list-ref lst) (map sub1 picks))])
             (values picked (remove* picked lst)))])))

;; sort-attend.v2.0
(define sort-attend.v2.0
  (lambda (lst picks)
    (let ([picked (map (curry list-ref lst) picks)])
      (values picked (remove* picked lst)))))

;; sort-attend.v1
(define sort-attend.v1
  (lambda (lst picks)
    (letrec
        ([n 1]
         [picked '()]
         [others '()]
         [fun (lambda (picks lst n picked others)
                 (cond [(null? lst)
                        (list (reverse picked)
                              (reverse others))]
                       [(null? picks)
                        (list (reverse picked)
                              (flatten (reverse (cons lst others))))]
                       [else (cond [(eq? n (car picks))
                                    (fun (cdr picks)
                                         (cdr lst)
                                         (+ 1 n)
                                         (cons (car lst) picked)
                                         others)]
                                   [else (fun picks
                                              (cdr lst)
                                              (+ 1 n)
                                              picked
                                              (cons (car lst) others))])]))])
      (fun picks lst n picked others))))

;; sub-roster: (roster . (list of ints)) -> (listof students)
;; consume roster and list of ints, return list of roster items
;; that correspond to (listof ints).
;; (list of ints) is item numbers, NOT index numbers. 
;; If zero (0) is anywhere in the list, it returns the original roster.
;; ex. (sub-roster '(Jamal Harris Squeek Frank) '(1 3)) -> '(Jamal Squeek)
;;     (sub-roster '(Jamal Harris Squeek Frank) '(1 0)) ->
;;       '(Jamal Harris Squeek Frank)
;; ex. (sub-roster '(Jamal Harris Squeek Frank) '(1 3)) -> '(Jamal Squeek)
;;     (sub-roster '(Jamal Harris Squeek Frank) '(1 0)) ->
;;       '(Jamal Harris Squeek Frank)
(define sub-roster
  (lambda (roster picks)
    (cond [(null? picks) '()]
          [(member 0 picks) roster]
          [else
           (map (lambda (x) (list-ref roster (- x 1)))
                 picks)])))

;; tests
#|
(print-roster (sub-roster)) roster '()
(print-roster (sub-roster
               (course-roster (hash-ref (car SEM) '101a)) '(2 0)))
print-roster weegl (slurpry  weesemean)
|#
