;;https://leanpub.com/lovinglisp/read#leanpub-auto-accessing-relational-databases
;;TODO: Figure out packaging
(load "/home/tasker/quicklisp/setup.lisp")
(ql:quickload "unix-opts")
(ql:quickload "split-sequence")
(ql:quickload "cl-dbi")
;(ql:quickload "local-time")
;(ql:quickload "usocket")

(defvar *db*)

;;;;Clear umask in order to mkfifo with appropriate permissions
#+sbcl (sb-posix:umask #O000)
 #-(or sbcl) (error "Not implemented yet")

(defun mkfifo (name mode)
  #+sbcl (sb-posix:mkfifo name mode)
  #-(or sbcl) (error "Not implemented yet"))

;Options to add:
;       location of task files
(opts:define-opts
  (:name :help
   :description "Print this help text."
   :short #\h
   :long "help")
  (:name :ifname
   :description "The name of the FIFO for tasks to read. Defaults to /tmp/to-task"
   :long "ififo-name"
   :arg-parser #'identity
   :meta-var "NAME")
  (:name :ifname
   :description "The name of the FIFO for tasks to write. Defaults to /tmp/from-task"
   :long "ofifo-name"
   :arg-parser #'identity
   :meta-var "NAME")
  (:name :ifmode
   :description "File permissision for ififo. Defaults to 622."
   :long "ifmode"
   :arg-parser (lambda (arg) (parse-integer arg :radix 8))
   :meta-var "MODE")
  (:name :ofmode
   :description "File permissision for ififo. Defaults to 644."
   :long "ofmode"
   :arg-parser (lambda (arg) (parse-integer arg :radix 8))
   :meta-var "MODE")) 

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun index-of-1st-different-character (string1 string2)
    "Defined so that the following expression returns t
     (let ((idx (1- (index-of-1st-different-character string1 string2))))
       (string-equal (subseq string1 0 idx) (subseq string2 0 idx)))

     In other words, if idx is the return value, the first idx-1 characters of both strings are the same (case insensitive)"
    (loop for c1 across string1
          for c2 across string2
          for i from 0
          unless (char-equal c1 c2) return (1+ i) ;1+ to include the different character
          ;+2 here becuase loop hasn't incremented i and we have to add 1 on top of it
          finally (return (min (length string1) (+ i 2))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-shortest-unique-initial-substrings (strings)
    "Given a list of strings, return the shortest unique initial substring of each.
     This function is case-insensitive.

     E.g., (get-shortest-unique-initial-substring '(\"test\", \"tooth\", \"teeth\"))
     -> (\"tes\", \"to\", \"tee\")"
    (loop for s in strings
          for idx = (loop for str in strings
                          unless (eq str s) 
                          maximize (index-of-1st-different-character s str))
          collect (cons (subseq s 0 idx) idx))))

(defun starts-with (str1 str2)
  "Does str1 start with str2 (case-insensitive)?"
  (string-equal str1 str2 :start1 0 :end1 (length str2)))

(defmacro dispatch-on-unique-prefix (&rest args)
  (let ((shortest-names (get-shortest-unique-initial-substrings args)))
    `(let ((line (split-sequence:split-sequence (code-char 0)
                                                line
                                                :remove-empty-subseqs t)))
       (cond ,@(loop for arg in args
                     for short in shortest-names
                                         ;;Allow the shortest unambigious initial substring to identify a command
                     collect (list `(and (string-equal ,(car short) (car line) :start2 0 :end2 ,(cdr short))
                                         (starts-with ,arg (car line)))
                                   `(,(intern (string-upcase arg)) fifo
                                                                   ;;car is the "script" name
                                                                   ;;we only care about the arguments
                                                                   (cdr line))))
             (t (does-not-exist fifo line))))))

(defmacro defcommand (name &body body)
  "Wraps command in a handler-case, and injects two arguments, fifo and args."
  `(defun ,name (fifo args)
     (handler-case (progn
                     ,@body)
       (condition (c) (write-to-fifo fifo "Something went wrong in ~a called with ~a: ~%~a" ',name args c)))))

(defun print-items (rows &optional fields)
  "Pretty print the returned database rows.
   If fields is provided, only print the request fields.
   
   E.g., (print-items rows '(:|rowid| :due-date :notes)) => \"rowid 10 DUE-DATE 2019-01-01 00:00:00 NOTES Write New Year's Resolutions\""
  (format nil "~%~{~{~a ~a     ~}~%~}" 
          (loop for row in rows
                if fields
                   collect (loop for field in fields
                                 appending (if (eq field :rowid) 
                                               (list field (get row :|rowid|)) ;Why must rowid be different?
                                               (list field (getf row field))))
                else
                   collect row)))

(defun write-to-fifo (fname str &rest args)
  "Opens the fifo and writes to it.
   STR is a control string for FORMAT, and ARGS are any arguments for the control string.
   
   N.B. This blocks until something read from the fifo."
  (with-open-file (output fname :direction :output :if-does-not-exist :error)
    (format output "~&~?~&" str args)))

(defun list-tasks (fifo args)
  (declare (ignore args))
  (let ((get-all (cl-dbi:prepare *db* "SELECT rowid, * from tasks")))
    (write-to-fifo fifo (print-items (cl-dbi:fetch-all (cl-dbi:execute get-all))))))

(defun insert-item (db &key due-date project tags plist notes)
  "Inserts a new task into the database.
   Created-at and last-updated are set to now. Completed is set to 0."
  (let ((q (cl-dbi:prepare db "INSERT INTO tasks VALUES(datetime('now'), ?, datetime('now'), ?, ?, ?, ?, 0)")))
    ;;TODO: Check all types
    (cl-dbi:execute q (or due-date "")
                      (or project "")
                      (or tags "")
                      (or plist "")
                      (or notes ""))))

(defcommand add-task
  (if args
      (progn
        (let ((*read-eval* nil))
          (apply #'insert-item *db* (loop for (key arg) on args by #'cddr
                                          appending (list (read-from-string key) arg))))
        (write-to-fifo fifo "done" args))
      (write-to-fifo fifo "Empty task - Not Inserted.")))

(defun modify-item (db &key (rowid nil rowid-p) due-date project tags plist notes (completed nil completed-p))
  (if rowid-p
      (let ((oldrow (cl-dbi:fetch (cl-dbi:execute (cl-dbi:prepare db "SELECT * from tasks where rowid = ?") rowid)))
            (statement (cl-dbi:prepare db "UPDATE tasks SET 'due-date' = ?,
                                                            project = ?,
                                                            tags = ?,
                                                            plist = ?,
                                                            notes = ?,
                                                            completed = ?,
                                                            'last-updated' = datetime('now')
                                                        WHERE rowid = ?")))
        (when oldrow
          (cl-dbi:execute statement (or due-date (getf oldrow :due-date))
                                    (or project (getf oldrow :project))
                                    (or tags (getf oldrow :tags))
                                    (or plist (getf oldrow :plist))
                                    (or notes (getf oldrow :notes))
                                    (if completed
                                        1
                                        (if completed-p ;I'll have to add support for uncompleting a task
                                            0
                                            (getf oldrow :completed)))
                                    rowid))
        "done")
    (error "Must supply rowid.")))

(defcommand modify-task
  (let ((*read-eval* nil))
    (write-to-fifo fifo (apply #'modify-item *db* (loop for (key arg) on args by #'cddr
                                                        appending (list (read-from-string key) arg))))))

(defun complete-task (fifo args)
  (modify-task fifo (append args '(:completed t))))

(defun flush-tasks (fifo args)
  (declare (ignore fifo args))
  ;;get all incomplete tasks
  ;;close the database connection
  ;;move the database file
  ;;open a new connection
  ;;insert all tasks
  )

;;;; With proper packaging, I can use quit and exit
(defun quitl (fifo args)
  (declare (ignore fifo args))
  (throw 'quit nil))

(defun exitl (fifo args)
  (declare (ignore fifo args))
  (throw 'quit nil))

(defun does-not-exist (fifo line)
  (write-to-fifo fifo "Unknown command ~A" line))

(defun dispatch (fifo line)
  "Parses line to determine action. Writes any necesary output to fifo."
  (dispatch-on-unique-prefix
    "list-tasks"
    "add-task" 
    "modify-task" 
    "complete-task" 
    ;"flush-tasks" 
    "quitl"
    "exitl"))

(defun parse-args ())

(defmacro with-open-fifo-for-read ((binding fname mode) &body body)
  `(progn
     (mkfifo ,fname ,mode)
     (unwind-protect
       ;;Opening a fifo as :io is technically undefined
       ;;But according to the internet, it's expected to work on Linux
       (with-open-file (,binding ,fname :direction :io :if-exists :append) 
         ,@body)
       (delete-file ,fname))))

(defun startup ()
  (parse-args)
  (let ((output "/tmp/tasks-output"))
    (mkfifo output #O644)
    (unwind-protect
      (with-open-fifo-for-read (input "/tmp/tasks-input" #O622)
        (setf *db* (cl-dbi:connect :sqlite3 :database-name "tasks.db"))
        (cl-dbi:execute (cl-dbi:prepare *db* "CREATE TABLE IF NOT EXISTS tasks(
                                             'CREATION-TIME' TEXT,
                                             'DUE-DATE' TEXT,
                                             'LAST-UPDATED' TEXT,
                                             PROJECT TEXT,
                                             TAGS TEXT,
                                             PLIST TEXT,
                                             NOTES TEXT,
                                             COMPLETED INTEGER)"))
        (catch 'quit
                 (loop for line = (read-line input)
                       do (dispatch output line)))
        (write-to-fifo output "Exiting"))
      (delete-file output))))

(startup)
