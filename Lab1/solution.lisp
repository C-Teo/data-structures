(defun add-3 (x)
  (+ x 3))

(defun square (x)
  (* x x))

(defun my-abs (x)
  (abs x))

(defun largest (x y)
  (if (> x y)
      x
      y))

(defun dep (b a)
  (if (< a 10000)
      (+ b a)
      nil))

(defun wdr (b a)
  (if (and (>= b a) (< a 10000))
      (- b a)
      (+ b a)))

(defvar *balance* 100)

(defun withdraw (amount)
  (cond ((< amount 0) (print "Negative amount"))
        ((>= amount 10000) (print "Exceeds maximum withdrawal amount"))
        ((< *balance* amount) (print "Insufficient funds"))
        ((/= 0 (mod amount 20)) (print "Amount not multiple of 20"))
        (t (:= *balance* (- *balance* amount))))
  *balance*)

(defun deposit (amount)
  (cond ((< amount 0) (print "Negative amount"))
        ((>= amount 50000) (print "Exceeds maximum deposit amount"))
        (t (:= *balance* (+ *balance* amount))))
  *balance*)
