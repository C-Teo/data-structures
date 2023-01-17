(defun count-nums (a)
  (cond ((null a) 0)
        ((listp (first a)) (+ (count-nums (first a))(count-nums (rest a))))
         (t (+ 1 (count-nums (rest a))))))

; Structures

(defstruct movie
  title director year type)

(let ((exmovie (make-movie :title "CPS305" :director "Marcus Santos")))
  (setf (movie-year exmovie) 1985)
  (values (movie-title exmovie)
          (movie-director exmovie)
          (movie-year exmovie)))

(defparameter *size* 10) 

(defvar *db*)

(setf *db* (make-array *size* :initial-element nil))

(defun add-movie (m)
  "Adds a movie to the database and returns true. Otherwise, returns NIL.
Notice: adds duplicates!!!"
  (dotimes (i *size*)
    (unless (aref *db* i)
      (setf (aref *db* i) m)
      (return t))))

(defun in-db? (title)
  (dotimes (i (length *db*))
    (when (and (typep (aref *db* i) 'movie) (equal title (movie-title (aref *db* i))))
      (return-from in-db? t))))

; Arrays

#(1 2 3 4)
(make-array 3)
(make-array 5 :initial-element nil)

; Lists



; Searching and Sorting Algorithms

(defun seq-search (val vec)
  (dotimes (i (length vec))
    (if (= (aref vec i) val)
        (return i))))

(defun bin-search (val vec &optional (pos 0))
  (let* ((midpt (floor (length vec) 2))
        (midel (aref vec midpt)))
    (if (= val midel) (return-from bin-search (+ pos midpt))
        (cond ((< (length vec) 2) nil)
              ((< midel val)(bin-search val (rtl:slice vec midpt) (+ pos midpt)))
              ((> midel val)(bin-search val (rtl:slice vec 0 midpt)))))))

(defun sel-sort (vec comp)
  (dotimes (cur (- (length vec) 1))
    (let ((best (aref vec cur))
          (index cur))
      (do ((i cur (+ i 1)))
          ((>= i (length vec)))
        (when (funcall comp (aref vec i) best)
          (setf best (aref vec i)
                index i)))
      (rotatef (aref vec cur) (aref vec index))))
  vec)

(defun insertion-sort (vec comp)
  (dotimes (i (- (length vec) 1))
    (do ((j i (1- j))) ; starts at i, decrements by 1
        ((minusp j)) ; checks if j is negative
      (if (funcall comp (aref vec (1+ j)) (aref vec j))
          (rotatef (aref vec (1+ j)) (aref vec j))
          (return))))
  vec)

(defun ins-sort (vec comp)
  (dotimes (cur (- (length vec) 1))
    (do ((j cur (- j 1)))
        ((minusp j))
      (if (funcall comp (aref vec (+ j 1)) (aref vec j))
          (rotatef (aref vec (+ j 1)) (aref vec j))
          (return))))
  vec)
