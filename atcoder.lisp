(defclass union-find ()
  ((parent :accessor parent :initarg :parent :initform nil)
   (rank :accessor rank :initarg :rank :initform nil)))

(defun make-union-find (size)
  "Initialize the union-find structure with `size` elements."
  (let ((uf (make-instance 'union-find
                           :parent (make-array size :initial-contents (loop for i from 0 below size collect i))
                           :rank (make-array size :initial-element 0))))
    uf))

(defmethod uf-find ((uf union-find) x)
  "Find the root of element `x` with path compression."
  (let ((par (parent uf)))
    (if (/= (aref par x) x)
        (setf (aref par x) (uf-find uf (aref par x))))
    (aref par x)))

(defmethod unite ((uf union-find) x y)
  "Unite the sets containing `x` and `y`."
  (let* ((root-x (uf-find uf x))
         (root-y (uf-find uf y))
         (par (parent uf))
         (rnk (rank uf)))
    (cond ((/= root-x root-y)
           (cond ((> (aref rnk root-x) (aref rnk root-y))
                  (setf (aref par root-y) root-x))
                 ((< (aref rnk root-x) (aref rnk root-y))
                  (setf (aref par root-x) root-y))
                 (t
                  (setf (aref par root-y) root-x)
                  (incf (aref rnk root-x))))))))

(defmethod same-p ((uf union-find) x y)
  "Check if `x` and `y` are in the same set."
  (= (uf-find uf x) (uf-find uf y)))

(defun solve ()
  "Main function to handle inputs and process queries."
  (let ((n (read))  ; Read the size 'n' from input
        (q (read))) ; Read the number of queries 'q' from input
    (let ((uf (make-union-find n)))  ; Create a union-find instance with size 'n'
      (dotimes (_ q)
        (let ((type (read))
              (u (read))
              (v (read)))
          (cond ((zerop type)
                 (unite uf u v))
                (t
                 (format t "~:[0~;1~]~%" (same-p uf u v)))))))))

(solve)
