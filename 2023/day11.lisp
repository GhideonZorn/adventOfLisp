(defparameter *space* 2)

(defun parse-lines (stream-in)
  (let ((line (read-line stream-in nil)))
    (if (null line)
        nil
        (cons line (parse-lines stream-in)))
    ))

(defun get-array-and-start (lines-list)
  (let* ((m (length (car lines-list)))
         (n (length lines-list))
         (array (make-array (list n m))))

    (loop for y from 0 below n do
      (loop for x from 0 below m do
        (setf (aref array y x) (char (nth y lines-list) x))
          ))
    array))

;; All coords are in form (y x)
(defun only-space (univers pos inc dimensions)
  (if (or (eq (nth 0 pos) (nth 0 dimensions))
          (eq (nth 1 pos) (nth 1 dimensions)))
      t
      (if (eq #\# (aref univers (nth 0 pos) (nth 1 pos)))
          nil
          (only-space univers (mapcar #'+ pos inc) inc dimensions))
      ))

(defun diff (p1 p2)
  (abs (- p1 p2)))

(defun get-direction (x xref)
  (if (> x xref)
      -1
      1))

(defun calcul-galaxie-distance (g1 g2)
  (+ (diff (nth 0 g1) (nth 0 g2)) (diff (nth 1 g1) (nth 1 g2))))

(defun calcul-galaxies-distances (univers galaxies)
  (reduce #'+ (loop for src in galaxies append
        (loop for dst in galaxies
          when (or (not (eq (nth 0 src) (nth 0 dst)))
                   (not (eq (nth 1 src) (nth 1 dst))))
            collect (calcul-galaxie-distance src dst)))))

(defun get-galaxies (univers real-x real-y m n)
  (loop for y from 0 below (nth 0 (array-dimensions univers))
        append (loop for x from 0 below (nth 1 (array-dimensions univers))
                    do (if (only-space univers (list 0 x) (list 1 0) (list m n))
                        (setf real-x (+ real-x *space*))
                        (setf real-x (+ real-x 1)))
                    when (eq (aref univers y x) #\#)
                      collect (list real-y real-x))
        do (if (only-space univers (list y 0) (list 0 1) (list m n))
            (setf real-y (+ real-y *space*))
            (setf real-y (+ real-y 1)))
        do (setf real-x 0)
  ))

(defun compute-result (file)
  (with-open-file (file-stream file)
    (let ((univers (get-array-and-start (parse-lines file-stream))))

      (/ (calcul-galaxies-distances univers
          (get-galaxies univers 0 0 (nth 0 (array-dimensions univers))
                            (nth 1 (array-dimensions univers)))) 2)
      )))

(format T "Day 11 part 1: ~d~%" (compute-result "day11.txt"))
(setf *space* 1000000)
(format T "Day 11 part 2: ~d~%" (compute-result "day11.txt"))
