(ql:quickload :cl-csv)

(defstruct std
  email
  (qscores (list))
  flag
  gform)

(defun load-form (file)
  (cl-csv:read-csv file))

(defun parse (gscore)
  "Converts the Google form score string into a number score out of 100"
  (let ((r (with-input-from-string (stream gscore)
	     (loop with eof-marker = '#:eof
		   for object = (read stream nil eof-marker)
		   until (eq object eof-marker)
		   collect object))))
    (* (/ (car r) (caddr r)) 100)))

(defun process-class-responses (lr classScores filename)
  (dolist (r lr)
    (let* ((time (car r))
	   (email (cadr r))
	   (gscore (caddr r))
	   (astd (gethash email classScores)))
      (if astd
	  (setf (std-qscores astd)
		(cons (list time (parse gscore)) (std-qscores astd)))
	  (setf (gethash email classScores) (make-std :email email
						      :qscores (cons (list time (parse gscore)) nil)
						      :gform filename))))))
			    
(defun process-quiz-folder (folder ht)
  "Obtains the scores of each quiz from the gforms in 'folder', inserts
them in a list, and stores them in hash-table ht indexed by the students email.
Returns the number of valid csv quiz files found in the folder."
  (let* ((fileslist (uiop:directory-files folder))
	 (count 0))
    (dolist (filename fileslist)
      (if (equal (pathname-type filename) "csv")
	  (progn
	    (format t "* Processing quiz file ~a..." filename)
	    (process-class-responses (cdr (load-form filename)) ht filename)
	    (format t "Done.~%")
	    (incf count))
	  (format t "!!! ~a is NOT a csv file. Ignoring it.~%" filename)))
    count))

(defun check-alien-emails (ht)
  (let ((flag nil))
    (loop for email being each hash-key of ht using (hash-value std-email) do
      (unless (std-flag (gethash email ht))
	(setf flag t)
	(format t "!!! The provided d2l grades file does not contain the student email <~a> mentioned in the csv file ~a~%" email (std-gform (gethash email ht)))))
    (unless flag
      (format t "No issues creating grades d2l file.~%"))))

(defun gf2d2l (d2lFile quizzesFolder)
  "Based on the quizzes marks d2lFile file exported by d2l and the google forms quizzes
contained in the quizzesFolder, obtains the total quizzes' scores for each student listed in
d2lFile, and creates a d2l-uploadable file 'd2l-quiz-scores.csv' for the students' quizzes scores."
  (let* ((d2lPathname (pathname d2lFile))
	 (d2llis (cl-csv:read-csv d2lPathname))
	 (ht (make-hash-table :test 'equal))
	 (nofq (process-quiz-folder quizzesFolder ht)))
    (with-open-file (stream "d2l-quizzes-marks.csv" :direction :output :if-exists :supersede  :if-does-not-exist :create)
      (format stream "~{~a~^,~}~%" (car d2llis)) ;; generates csv file header
      (dolist (csvr (cdr d2llis))
	(let* ((uname (car csvr))
	       (fstname (cadr csvr))
	       (lstname (caddr csvr))
	       (email (cadddr csvr))
	       (stdScore (loop for qres in (std-qscores (gethash email ht)) 
			       sum (cadr qres) into s
			       finally (return s))))
	  (setf (std-flag (gethash email ht)) t)
	  (format stream "~a,~a,~a,~a,~a,#~%" uname fstname lstname email (/ stdScore nofq)))))
    (check-alien-emails ht)))




