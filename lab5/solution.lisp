(defun bracket-checker (list)
  (let ((stack nil))
    (dolist (elem list (null stack))
      (case elem
        ([ (push elem stack))
        (] (if (null stack)(return)(pop stack)))))))

(defun fact (x)
  (if (> x 1)
      (* x (fact (- x 1)))
      1))

(defun arith-eval (expr)
  "EXPR is a list of symbols that may include:
square brackets, arithmetic operations, and numbers."
  (print (bracket-checker expr))
  (when (not (bracket-checker expr))(return-from arith-eval (print "Error")))
  (let ((ops ())
        (vals ())
        (op nil)
        (val nil))
    (dolist (item expr)
      (case item
        ([ ) ; do nothing
        ((+ - * / ^ SDIV MAXF FACT) (push item ops))
        (] (setf op (pop ops) val (pop vals))
         (case op
           (+ (setf val (+ val (pop vals))))
           (- (setf val (- (pop vals)  val)))
           (* (setf val (* val (pop vals))))
           (/ (setf val (/ (pop vals)  val)))
           (^ (setf val (expt (pop vals) val)))
           (SDIV (let ((y (pop vals))) (setf val (/ (- (pop vals) y) val))))
           (MAXF (setf val (max val (pop vals) (pop vals))))
           (FACT (setf val (fact val))))
         (push val vals))
        (otherwise (push item vals))))
    (pop vals)))
