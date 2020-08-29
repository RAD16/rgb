;#lang racket

(require racket/serialize)
(require racket/include)
(require racket/list)
(include "io-rgb.rkt")

;;; RGB: the RAD GradeBook

#|
	DATA STRUCTURES as STRUCTS and LISTS
      Semesters (listfo semester hash-tables): list of semesters
      Semester (hash) hash table of courses
      Course (struct): title semester CRN section roster
      Roster (listof student structs): student-1 student-2 student-3 ... 
      Student (struct): name email course id gradebook attendance
      Gradebook (listof assignment structs): assignment-1 assignment-2 assignment-3 ... 
      Assignment (struct): title deadline grade late?
      Attendance (listof session pairs): session-1 session-2 session-3 ... 
		  Session    (pair): session-date attend? 
|#


#|
   TODO
     - given a roster, print all students present on given day
     - absences counter built into student struct
     - find student by searching across courses/rosters
     - UI work

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

;; ?? dafuq is going on here? 
;; find-course: ((course-hash) . string) -> (student?)
;; Find Course by title
;; search for course in hash-table.
;; Consumes a hash table and a course's title as a string,
;; returns a matching course struct.
;; ex. find-course (car SEM) '101a) roster "Rob" -> Rob<#student>
(define find-course
  (lambda (roster student)
    (or (findf (lambda (x)
                 (equal? student (student-last x))) roster)
        (printf "~a not a last name in roster.\n" student))))

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

;; add-student!:
;; (course title . student label) -> (void)
;; consumes a course struct, student struct, and adds the student
;; onto the roster list of the course.
(define add-student!
	(lambda (student course)
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
;; ex. (find-student-ln roster "Willer") -> Willer<#student>
;;
;; Star (*) version takes a list of name strings and returns multiple student structs
;; ex. (find-student-ln* roster '("Rob" "Karen")) -> '(Rob<#student> Karen#<student>)
(define find-student-ln
  (lambda (student roster)
    (or (findf (lambda (x)
                 (equal? student (student-last x))) roster)
        (printf "~a not a last name in roster.\n" student))))

(define find-student-ln*
  (lambda (students roster)
    (or (map (lambda (x) (find-student-ln roster x)) students)
        (printf "~a not a last name in roster.\n" student))))

;; find-student-fn: ((listof student) . string) -> (student?)
;; Find Student by First Name
;; search for student in roster by first name.
;; Consumes a roster and a student's first name as a string,
;; returns a matching student struct.
;; ex. (find-student roster "Rob") -> Rob<#student>
;; ex. (print-student (find-student roster "Rob")) -> printed info about Rob 
;;
;; Star (*) version takes a list of name strings and returns multiple student structs
;; ex. (find-student roster '("Rob" "Karen")) -> '(Rob<#student> Karen#<student>)
(define find-student-fn
  (lambda (student roster)
    (or (findf (lambda (x)
                 (equal? student (student-first x))) roster)
        (printf "~a not a first name in roster.\n" student))))

(define find-student-fn*
  (lambda (students roster)
    (or (map (lambda (x) (find-student-fn x roster)) students)
        (printf "~a not a first name in roster.\n" student))))

;; find-student-id: ((listof student) . string) -> (student?)
;; Find Student by ID number
;; search for student in roster by student ID number.
;; Consumes a roster and a student's ID number as an exact number.
;; returns a matching student struct.
;; ex. (print-student (find-student roster 34)) -> printed info about Floppy 
;;
;; Star (*) version takes a list of name strings and returns multiple student structs
;; ex. (find-student roster '("Rob" "Karen")) -> '(Rob<#student> Karen#<student>)
(define find-student-id
  (lambda (student roster)
    (or (findf (lambda (x)
                 (equal? student (student-id x))) roster)
        (printf "~a not a student ID number in roster.\n" student))))

(define find-student-id*
  (lambda (students roster)
    (or (map (lambda (x) (find-student-id roster x)) students)
        (printf "~a not a student ID number in roster.\n" student))))

;; find-student-index; (roster . first-name) -> (index)
;; consumes a roster and a first name, returns their index number
;; For use with (find-student-number) to produce absentee lists, etc.
;; (find-student-index '(Rob Sherryl Donte) "Sherryl") -> 1
;; (find-student-index '(Rob Sherryl Donte) "Willy")
;;      -> "Willy not a first name in roster"
;; Star (*) version takes a list of first names,
;; and returns a list of roster numbers
;; (find-student-number* '(Rob Sherryl Donte) '("Sherryl" "Donte")) -> '(2 3)
(define find-student-index
  (lambda (name roster)
    (index-of roster (find-student-fn roster name))))

(define find-student-index*
  (lambda (names roster)
    (map (lambda (x) (find-student-index roster x)) names)))


;; find-student-number; (roster . first-name) -> (roster number)
;; consumers a roster and a first name, returns the roster number
;; NOT the index. For use with (sub-roster) to produce absentee lists, etc.
;; (find-student-number '(Rob Sherryl Donte) "Sherryl") -> 2
;; (find-student-number '(Rob Sherryl Donte) "Willy")
;;      -> "Willy not a first name in roster"
;;
;; Star (*) version takes a list of first names,
;; and returns a list of roster numbers
;; (find-student-number* '(Rob Sherryl Donte) '("Sherryl" "Donte")) -> '(2 3)
(define find-student-number
  (lambda (name roster)
    (add1 (find-student-index roster name))))

(define find-student-number*
  (lambda (names roster)
    (map (lambda (x) (find-student-number roster x)) names)))

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

;; user-assignment-info: (void) -> (listof assignment info)
;; takes no args, calls (make-prompt) to solicit user input
;; to create a list of user inputs
(define user-assignment-info
	(lambda ()
    (make-prompt
     "Assignment title: "
     "Deadline (e.g. 20200901): "
     "Score: "
     "Late: ")))

;; create-assignment!: (listof assignment info) -> assignment struct
;; consumes a listof info corresponding to an assignment struct,
;; returns an assignment struct
;; (create-assignment! '("Essay" 20200901 95 #f)) -> assignment struct
(define create-assignment
	(lambda (assign-info)
    (assignment
     (list-ref assign-info 0) ; title
     (list-ref assign-info 1) ; deadline
     (list-ref assign-info 2) ; score
     (list-ref assign-info 3)))) ; late?

;; add-assignment!: (student struct . assignment struct) -> (void)
;; consumes student and assignments structs, sets the student gradebook
;; with the new list of assignments (gradebook)
;; ex. (add-assignment! (create-assignment (user-assignment-info)))
(define add-assignment!
	(lambda (assignment student)
    (set-student-gradebook! student 
                            (cons assignment (student-gradebook student))))) 

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
  (lambda (assignment gradebook)
    (or (findf (lambda (x)
                 (equal? assignment (assignment-title x))) gradebook)
        (printf "~a not an assignment in gradebook.\n" assignment))))

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
	(lambda (session student)
    (set-student-attendance! student 
      (cons session (student-attendance student))))) 

;; batch-add-session!: (session . roster) -> (void)
;; consumes a session pair and a roster list,
;; adds a session to every student in roster
(define batch-add-session!
  (lambda (session roster)
    (map (lambda (x) (add-session! session x)) roster)))

;; print-attend: (student) -> (void)
;; consumes a student struct, prints attendance info
;; as (session . status) pairs
(define print-attend
  (lambda (student)
    (for-each (lambda (x) (printf "~a\n" x)) (student-attendance student))))

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

;; absent?: (date) -> boolean
;; consumes a date as int, returns true
;; if an associated pair evals to 'absent.
;; Intended for use to return lists of absent students
;; given a date. 
(define absent?
  (lambda (date)
    (equal? 'absent (cadr date))))

;; late?: (date) -> boolean
;; consumes a date as int, returns true
;; if an associated pair evals to 'late.
;; Intended for use to return lists of late students
;; given a date. 
(define late?
  (lambda (date)
    (equal? 'late (cadr date))))

;; get-absent: (listof students . date) -> (listof students)
;; consumes a roster, and returns a new roster
;; with only the students who have been absent on date.
;; ex. (get-absent '(Rob Karen Sherryl) 20200901) -> '(Sherryl)
(define get-absent
  (lambda (date roster)
    (filter (lambda (x) (absent?
                         (assoc date (student-attendance x))))
            roster)))

;; get-late: (listof students . date) -> (listof students)
;; consumes a roster, and returns a new roster (listof student-structs)
;; with only the students who have been late on date.
;; ex. (get-late '(Rob Karen Sherryl) 20200901) -> '(Sherryl)
(define get-late
  (lambda (date roster)
    (filter (lambda (x) (late?
                         (assoc date (student-attendance x))))
            roster)))

;; count-absences: (student) -> int
;; consumes a student struct, returns number of absences
;; in a student's attendance report.
;; (count-absences <student>) -> 3
(define count-absences
  (lambda (student)
    (length (filter (lambda (x)
                   (equal? 'absent (cadr x)))
                 (student-attendance student)))))

;; count-lates: (student) -> int
;; consumes a student struct, returns number of latenessnes
;; in a student's attendance report.
;; (count-lates <student>) -> 3
(define count-lates
  (lambda (student)
    (length (filter (lambda (x)
                      (equal? 'late (cadr x)))
                    (student-attendance student)))))

;; run-attendance-number: (roster day noshows tardy) -> batch-runs attendance
;; consume roster, day then aleady-parsed sub-rosters as lists of numbers
;; adds correct session to each student's attendance
(define run-attendance-number
  (lambda (roster day noshows tardy)
    (let* ([states (list `(,day absent) `(,day late) `(,day present))]
           [groups (list
                    (sub-roster noshows roster)
                    (sub-roster tardy roster)
                    (sub-roster-inv 
		      (append noshows tardy) roster))])
      (map batch-add-session! states groups))))

;; run-attendance-fn: (roster day noshows tardy) -> batch-runs attendance
;; consume roster, day then two lists of first names as strings 
;; adds correct session to each student's attendance
(define run-attendance-fn
  (lambda (roster day noshows tardy)
    (let* ([states
             (list `(,day absent) `(,day late) `(,day present))]
           [not-in-class
             (find-student-fn* noshows roster)]
           [late-to-class
             (find-student-fn* tardy roster)]
           [in-class
             (remove* (append not-in-class late-to-class) roster)])
      (map batch-add-session!
	   states
           (list not-in-class late-to-class in-class)))))

;; sub-roster: (roster . (list of ints)) -> (listof students)
;; consume roster and list of ints, return list of roster items
;; that correspond to (listof ints).
;; (list of ints) is item numbers, NOT index numbers. 
;; If zero (0) is anywhere in the list, it returns the original roster.
;; Consuming an empty list in picks returns an empty list.
;; ex. (sub-roster '(Jamal Harris Squeek Frank) '(1 3)) -> '(Jamal Squeek)
;;     (sub-roster '(Jamal Harris Squeek Frank) '(1 0)) ->
;;       '(Jamal Harris Squeek Frank)
(define sub-roster
  (lambda (picks roster)
    (cond [(null? picks) '()]
          [(member 0 picks) roster]
          [else
           (map (lambda (x) (list-ref roster (- x 1)))
                 picks)])))

;; sub-roster-invert: (roster . (list of ints)) -> (listof students)
;; consume roster and list of ints, return list of roster items
;; that DO NOT correspond to (listof ints).
;; (list of ints) is item numbers, NOT index numbers. 
;; If zero (0) is anywhere in the list, it returns the original roster.
;; Consuming an empty list in picks returns an empty list.
;; ex. (sub-roster '(Jamal Harris Squeek Frank)
;;                 '(1 3)) -> '(Harris Frank)
;;     (sub-roster '(Jamal Harris Squeek Frank) '(1 0)) ->
;;       '(Jamal Harris Squeek Frank)
;;     (sub-roster '(Jamal Harris Squeek Frank) '()) -> '()
(define sub-roster-inv
  (lambda (picks roster)
    (cond [(null? picks) '()]
          [(member 0 picks) roster]
          [else
           (map (lambda (x) (list-ref roster (- x 1)))
                (remove* picks
                         (map add1
                              (build-list (length roster) values))))])))

;; pick-student: (listof student) -> (void)
;; offers user a list of students, user enters number,
;; function prints the selected student.
(define pick-student
	(lambda (roster)
		(let* ([n 1])
      (for-each (lambda (n roster)
                  (printf "~a. ~a ~a\n" n
                          (student-first roster)
                          (student-last roster)))
                (map add1 (build-list (length roster) values))
                roster))
    (printf "Selection: \n")
    (display (student-first (list-ref roster (sub1 (read)))))))

(define pick-student.v1
	(lambda (roster)
		(letrec ([n 1]
             [fun (lambda (n roster)
                (cond [(null? roster) empty]
                      [else (printf "~a. ~a ~a.\n" n
                                    (student-first (car roster))
                                    (student-last (car roster)))
                        (fun (add1 n) (cdr roster))]))]) 
            (fun n roster))
    (printf "Selection: \n")
    (display (student-first (list-ref roster (sub1 (read)))))))

;; sort-attend: (roster . (listof ints)) -> (listof picks) (listof others)
;; consume roster and list of numbers,
;; return two lists: (listof (students who match "picks")).
;;                   and (listof (students who do NOT match "picks"))
;; (listof ints) is the item number, NOT the index.
;; ex. (sort-attend '('Jamal 'Lashawn 'Ibrahim) '(1 3)) -> '('Jamal 'Ibrahim)
;;                                                         '(Lashawn)
(define sort-attend
  (lambda (picks lst)
    (cond [(null? picks) '()]
          [(eq? 0 (car picks)) lst]
          [else
           (let ([picked (map (curry list-ref lst) (map sub1 picks))])
             (values picked (remove* picked lst)))])))

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
