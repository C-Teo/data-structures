(defstruct movie
  title  director year type)

(defparameter *size* 3) 

(defvar *db*)

(setf *db* (make-array *size*  :initial-element nil))

(defun add-movie (m)
  "Adds a movie to *db* and returns *db*"
  (if (typep (aref *db* (- *size* 1)) 'movie) nil
  (if (typep (in-db? (movie-title m)) 'array) nil
  (dotimes (i *size*)
    (when (null (aref *db* i))
      (setf (aref *db* i) m)
      (return *db*))))))

(defun in-db? (title)
  "Returns *db* if movie title is in the database; otherwise returns NIL"
  (dotimes (i *size*)
    (when (and (typep (aref *db* i) 'movie)
               (equal (movie-title (aref *db* i)) title))
      (return *db*))))

(defun delete-movie (title)
  "Deletes the movie from *db* if it is in the databse; otherwise returns NIL"
  (dotimes (i *size*)
    (when (and (typep (aref *db* i) 'movie)
               (equal (movie-title (aref *db* i)) title))
      (setf (aref *db* i) nil)
      (do ((c i (+ c 1))) ((>= (+ c 1) *size*) *db*)
        (progn (setf (aref *db* c) (aref *db* (+ c 1)))(setf (aref *db* (+ c 1)) nil)))
      (return *db*))))

(defun replace-movie (m nm)
  (if (in-db? (movie-title nm)) nil
      (if (in-db? (movie-title m))
          (dotimes (i *size*)
            (when (and (typep (aref *db* i) 'movie)
                       (equal (movie-title (aref *db* i)) (movie-title m)))
              (setf (aref *db* i) nm)
              (return t))
          nil))))

(defun num-movies ()
  (let ((c 0))
    (dotimes (i *size*)
      (if (typep (aref *db* i) 'movie)(incf c)))
    c))

                     
