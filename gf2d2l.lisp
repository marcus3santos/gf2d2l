(defstruct std
  email
  (qscores (list))
  flag
  gform)

(defun read-csv-elem (s &optional (accum nil) (p 0))
  (cond ((or (null s) (eql (car s) #\,))
	 (values (coerce (reverse accum) 'string) p))
	((or (char= (car s) #\Return) (char= (car s) #\"))
	 (read-csv-elem (cdr s) accum (1+ p)))
	(t (read-csv-elem (cdr s) (cons (car s) accum) (incf p)))))
	  
(defun read-csv-line (lst &optional (accum nil))
  (if (null lst) (reverse accum)
      (multiple-value-bind (elem pos)
	  (read-csv-elem lst nil 0)
	(let ((rest (when (< (1+ pos) (length lst))
			(subseq lst (1+ pos)))))
	  (read-csv-line rest (cons elem accum))))))
			    
(defun read-csv (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil nil)
       while line
       collect (read-csv-line (coerce line 'list)))))

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
	    (process-class-responses (cdr (read-csv filename)) ht filename)
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


(defun convert-to-d2l (d2llis ht nofq)
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
	(format stream "~a,~a,~a,~a,~a,#~%" uname fstname lstname email (/ stdScore nofq))))))

(defun gf2d2l (d2lFile quizzesFolder)
  "Based on the quizzes marks d2lFile file exported by d2l and the google forms quizzes
contained in the quizzesFolder, obtains the total quizzes' scores for each student listed in
d2lFile, and creates a d2l-uploadable file 'd2l-quiz-scores.csv' for the students' quizzes scores."
  (let ((d2llis (read-csv d2lFile)))
    (if (= (length (car d2llis)) 6) ;; checks for correct 2dl format
	(let* ((ht (make-hash-table :test 'equal))
	       (nofq (process-quiz-folder quizzesFolder ht)))
	  (convert-to-d2l d2llis ht nofq)
	  (check-alien-emails ht))
	(progn (format t "!!! Invalid d2l grade exported file.")
	       (return-from gf2d2l)))))




