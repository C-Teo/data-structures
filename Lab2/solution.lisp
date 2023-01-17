(defun a-sum (x y)
  (if (> x y) 0
      (+ x (a-sum (+ x 1) y))))

(defun sum-odd (x y)
  (if (> x y) 0
      (if (= 0 (mod x 2))
          (sum-odd (+ x 1) y)
          (+ x (sum-odd (+ x 1) y)))))

(defun my-function (func)
  (funcall func 1))

(defun sigma (f n p)
  (if (> n p) 0
      (+ (funcall f n) (sigma f (+ n 1) p))))
