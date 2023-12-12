(defun parse-lines (stream-in)
  (let ((line (read-line stream-in nil)))
    (if (null line)
        nil
        (cons line (parse-lines stream-in)))
    ))

(defun get-array-and-start (lines-list)
  (let* ((m (length (car lines-list)))
         (n (length lines-list))
         (array (make-array (list n m)))
         (starting-point nil))

    (loop for y from 0 below n do
      (loop for x from 0 below m do
        (setf (aref array y x) (char (nth y lines-list) x))
        (if (eq (char (nth y lines-list) x) #\S)
            (setf starting-point (list x y)))
          ))
  (list array starting-point)))

(defun is-out (array x y)
  (or (or (< x 0) (>= y (nth 0 (array-dimensions array))))
      (or (< y 0) (>= x (nth 1 (array-dimensions array)))))
  )

(defun is-vertical-high (char)
  (or (eq char #\S) (eq char #\|) (eq char #\L) (eq char #\J)))

(defun is-vertical-low (char)
  (or (eq char #\S) (eq char #\|) (eq char #\7) (eq char #\F)))

(defun is-horizontal-right (char)
  (or (eq char #\S) (eq char #\-) (eq char #\L) (eq char #\F)))

(defun is-horizontal-left (char)
  (or (eq char #\S) (eq char #\-) (eq char #\7) (eq char #\J)))

(defun is-connected (array xb yb before xa ya)
  (let ((actual (aref array ya xa)))
    (if (eq (aref array ya xa ) #\.)
        nil
        (if (eq before #\S)
            t
            (if (and (> xb xa) (is-horizontal-left before)
                     (is-horizontal-right actual))
                t
                (if (and (< xb xa) (is-horizontal-right before)
                         (is-horizontal-left actual))
                    t
                    (if (and (> yb ya) (is-vertical-high before)
                             (is-vertical-low actual))
                        t
                        (if (and (< yb ya) (is-vertical-low before)
                                 (is-vertical-high actual))
                            t
                            nil))))))
  ))

(defun is-visited (array x y)
  (eq (aref array y x) #\V)
  )

;; Point where we were : (xb yb)
;; Point where we are  : (xa ya)
(defun trace-loop (array visited-array xb yb before xa ya i)
    (if (and (not (is-out array xa ya))
             (not (is-visited visited-array xa ya))
             (is-connected array xb yb before xa ya)
             (not (eq (aref array ya xa) #\S)))
        (let ((actual (aref array ya xa)))
          (setf (aref array ya xa) i)
          (setf (aref visited-array ya xa) #\V)
          (setf array (trace-loop array visited-array xa ya actual (1+ xa) ya (1+ i)))
          (setf array (trace-loop array visited-array xa ya actual (1- xa) ya (1+ i)))
          (setf array (trace-loop array visited-array xa ya actual xa (1+ ya) (1+ i)))
          (setf array (trace-loop array visited-array xa ya actual xa (1- ya) (1+ i))))
        )
  array
  )

(defun is-pipe (char)
  (or (eq char #\|) (eq char #\L) (eq char #\J)))

(defun count-horizontal (array visited x y inc h-found)
  (if (is-out array x y)
      0
      (if (and (eq #\V (aref visited y x)) (is-pipe (aref array y x)))
          (+ 1 (count-horizontal array visited (+ x inc) y inc t))
          (count-horizontal array visited (+ x inc) y inc h-found))
    ))

(defun count-and-fill (array dims visited)
  (let ((count 0))
    (loop for i from 0 below (nth 0 dims) do
        (loop for j from 0 below (nth 1 dims) do
          (if (not (eq #\V (aref visited i j)))
              (let ((count-hr (count-horizontal array visited (1+ j) i 1 nil))
                    (count-hl (count-horizontal array visited (1- j) i -1 nil)))

                (if (and (or (oddp count-hr) (oddp count-hl))
                         (not (eq 0 count-hr))
                         (not (eq 0 count-hl)))
                  (progn (print count-hr)
                         (setf count (1+ count))
                         (setf (aref array i j) #\I))
                  (setf (aref array i j) #\O)))
              )))
    (print array)
    count))

(defun is-in-loop (array x y)
  (if (or (is-out array x y) (eq (type-of (aref array y x)) 'STANDARD-CHAR))
      0
      (aref array y x)))

(defun max-neighboor (tiles-array x y)
  (max (is-in-loop tiles-array (1+ x) y) (is-in-loop tiles-array (1- x) y)
       (is-in-loop tiles-array x (1+ y)) (is-in-loop tiles-array x (1- y))))

(defun compute-result (file)
  (with-open-file (file-stream file)
    (let* ((parsed-input (get-array-and-start (parse-lines file-stream)))
           (tiles-array (nth 0 parsed-input))
           (x (nth 0 (nth 1 parsed-input)))
           (y (nth 1 (nth 1 parsed-input)))
           (visited-array (make-array (array-dimensions tiles-array))))
      (setf tiles-array (trace-loop tiles-array visited-array x y #\S (1+ x) y 1))
      (setf tiles-array (trace-loop tiles-array visited-array x y #\S (1- x) y 1))
      (setf tiles-array (trace-loop tiles-array visited-array x y #\S x (1+ y) 1))
      (setf tiles-array (trace-loop tiles-array visited-array x y #\S x (1- y) 1))

      (setf (aref tiles-array y x) (1+ (max-neighboor tiles-array x y)))
      (print (/ (aref tiles-array y x) 2))
      (print tiles-array)
      (setf (aref visited-array y x) #\V)
      visited-array)))

(defun compute-results (file)
  (with-open-file (file-stream file)
      (let* ((parsed-input (get-array-and-start (parse-lines file-stream)))
             (tiles-array (nth 0 parsed-input))
             (visited (compute-result file)))

        (print visited)
        (print (count-and-fill tiles-array (array-dimensions tiles-array) visited))
      )))
  
;;(compute-result "day10.txt")
(compute-results "day10.txt")
