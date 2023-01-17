(defun next-state (std state)
  (let ((nstate (assoc state std)))
    (when nstate
      (cdr nstate))))

(defun jump-state (std s x)
  (when (or (and (typep (next-state std x) 'list)(member s (next-state std x)))(equal (next-state std x) s))
    (when (next-state std s)
      (return-from jump-state (next-state std s))))
  x)

(defun next-fork (std x &optional (temp x))
  (when (typep temp 'list)
    (return-from next-fork temp))
  (return-from next-fork (next-fork std x (next-state std temp))))

(defun interleave (x y &optional (acc nil))
  (cond ((and (car x) (car y)) (return-from interleave (interleave (cdr x) (cdr y) (cons (car y) (cons (car x) acc)))))
        ((car x) (return-from interleave (append (reverse acc) x)))
        ((car y) (return-from interleave (append (reverse acc) y)))
        (t (return-from interleave (reverse acc)))))

(defun comb (n k)
  (when (< n k) (return-from comb nil))
  (if (or (= n k)(= 0 k))
      (return-from comb 1)
      (return-from comb (+ (comb (- n 1) k) (comb (- n 1) (- k 1))))))
