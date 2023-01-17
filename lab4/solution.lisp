(defstruct movie
  title  director year type)

(defparameter *size* 3)

(defvar *db* (make-array *size*  :initial-element nil))

(defvar *db-list* nil)  

(defun quicksort (vec comp)
  (when (> (length vec) 1)
    (let ((pivot-i 0)
          (pivot (aref vec (1- (length vec)))))
      (dotimes (i (1- (length vec)))
        (when (funcall comp (aref vec i) pivot)
          (rotatef (aref vec i)
                   (aref vec pivot-i))
          (incf pivot-i)))
      (rotatef (aref vec (1- (length vec)))
               (aref vec pivot-i))
      (quicksort (rtl:slice vec 0 pivot-i) comp)
      (quicksort (rtl:slice vec (1+ pivot-i)) comp)))
  vec)

(defun add-movie (m)
  "Adds a movie to the DB and returns true"
  (dotimes (i *size*)
    (when (null (aref *db* i))
      (setf (aref *db* i) m)
      (return *db*))))

(defun add-movie-list (m)
  "Adds a movie to the end of *db-list* and returns the list"
  (unless (and (equalp (in-db-list? (movie-title m)) *db-list*)(not (equalp *db-list* nil)))
    (setf *db-list* (cons m *db-list*))
    (return-from add-movie-list *db-list*)))

(defun num-movies ()
  (let ((c 0))
    (dotimes (i *size*)
      (if (typep (aref *db* i) 'movie)(incf c)))
    c))

(defun sort-title ()
  (if (typep (aref *db* 0) 'movie) 
      (quicksort (rtl:slice *db* 0 (num-movies)) #'(lambda (x y) (string< (movie-title x) (movie-title y))))))

(defun sort-year ()
  (if (typep (aref *db* 0) 'movie) 
      (quicksort (rtl:slice *db* 0 (num-movies)) #'(lambda (x y) (< (movie-year x) (movie-year y))))))

(defun in-db-list? (title)
  (dolist (i *db-list*)
    (when (and (typep i 'movie)
               (string-equal (movie-title i) title))
      (return-from in-db-list? *db-list*))))

(defun from-year (year)
  (let ((acc nil))
    (dolist (i *db-list*)
      (when (and (typep i 'movie)
                 (equal (movie-year i) year))
        (setf acc (cons i acc))))
  (return-from from-year acc)))
    
