(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun line-to-numbers-list (line)
  (mapcar #'(lambda (value) (parse-integer value))
  (remove-if #'(lambda (value) (equal (length value) 0))
        (cl-ppcre:split " " (nth 1 (cl-ppcre:split ":" line)))
        )))

(defun reduce-to-one (list)
  (if (eq list nil)
      ""
      (concatenate 'string (car list) (reduce-to-one (cdr list)))
  ))

(defun line-to-number (line)
  (parse-integer
  (reduce-to-one
  (remove-if #'(lambda (value) (equal (length value) 0))
        (cl-ppcre:split " " (nth 1 (cl-ppcre:split ":" line)))
        ))))

(defun parse-line (stream-in)
  (let ((line (read-line stream-in nil)))
    (if (null line)
        nil
        line
        )))

(defun get-time-distance-pairs (times distances)
  (if (eq times nil)
      nil
      (cons
       (list (car times) (car distances))
       (get-time-distance-pairs (cdr times) (cdr distances)))
  ))

(defun compute-race (time distance button-time)
  (if (> time (+ button-time (floor distance button-time)))
      1
      0))

(defun compute-races (time distance button-time)
  (if (>= button-time time)
      0
      (+
       (compute-race time distance button-time)
       (compute-races time distance (1+ button-time)))
  ))

(defun multiply-ways (pairs)
  (if (eq nil pairs)
      1
      (*
       (compute-races (nth 0 (car pairs)) (nth 1 (car pairs)) 1)
       (multiply-ways (cdr pairs)))
  ))

(defun compute-result (file)
  (with-open-file (file-stream file)
    (let ((times (line-to-numbers-list (parse-line file-stream)))
          (distances (line-to-numbers-list (parse-line file-stream))))
      (multiply-ways (get-time-distance-pairs times distances))
    )))

(defun compute-result-2 (file)
  (with-open-file (file-stream file)
    (let* ((time (line-to-number (parse-line file-stream)))
           (distance (line-to-number (parse-line file-stream)))
           (low-lim (get-low-limit time distance 0 time)))
      (1+ (- (- time low-lim) low-lim))
      )))

(defun get-low-limit (time distance begin end)
  (let ((mid (+ begin (floor (- end begin) 2))))
    (if (or (equal begin end) (eq (- end begin) 1))
        mid
        (if (> time (+ mid (floor distance mid)))
            (get-low-limit time distance begin mid)
            (get-low-limit time distance (1+ mid) end))
        )))

(format T "Day 6 part 1: ~d~%" (compute-result "day6.txt"))
(format T "Day 6 part 2: ~d~%" (compute-result-2 "day6.txt"))
