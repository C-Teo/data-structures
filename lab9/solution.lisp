(defun next-state (std state)
  (let ((nstate (assoc state std)))
    (when nstate
      (cdr nstate))))

(defun has-loop (g s &optional (acc nil))
  (cond ((null s) nil)
        ((member s acc) t)
        ((typep s 'list) (dolist (elem s)
                            (if (member elem acc)
                                (return t)
                                (let ((res (has-loop g (next-state g elem) (cons elem acc))))
                                  (if res
                                      (return res))))))
        (t (has-loop g (next-state g s) (cons s acc)))))
