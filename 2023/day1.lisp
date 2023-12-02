(defun get-calibration-value (line)
  (let ((fd -1)
        (ld -1)
        (pos 0))
    (loop for char across line do
        (cond
          ((digit-char-p char)
             (if (eq fd -1)
                (setf fd (- (char-int char) (char-int #\0)))
                ())
             (setf ld (- (char-int char) (char-int #\0))))

          ;; Set to nil for part 1.
          (t
           (let ((match_res
                   (return-if-match line pos
                        '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine")
                        '(1 2 3 4 5 6 7 8 9))))
             (cond ((not (eq match_res nil))
                        (if (eq fd -1)
                            (setf fd match_res)
                            ())
                        (setf ld match_res))
                  (t ()))
                  )))
          (setf pos (+ pos 1)))
    (+ (* fd 10) ld)))


;; loop over the string line start at start. If it matches
;; an string element of string_list, it returns the
;; associated item of item_list (any type)
(defun return-if-match (line start string_list item_list)
  (let ((pos 0))
    (loop for str in string_list do
      (if (>= (length line) (+ start (length str)))
          (if (string= str (subseq line start (+ start (length str))))
              (return-from return-if-match (nth pos item_list))
              nil)
          nil)
      (setf pos (+ pos 1))
    )))

(defun parse-line (stream-in)
  (let ((line (read-line stream-in nil)))
        (if (null line)
          0
          (+ (get-calibration-value line) (parse-line stream-in)))
        ))

(defun compute-result (file)
    (with-open-file (file-stream file)
      (parse-line file-stream)))

(format T "Day 1: ~d~%" (compute-result "day1.txt"))
