(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun parse-token (token)
  (cl-ppcre:split " " token))

;; For part 1
(defun check-game-validity (tokens red green blue)
  (let ((total_r 0)
        (total_g 0)
        (total_b 0)
        (game_id -1))
      (loop for token in tokens do
        (let ((pair (parse-token (string-trim " " token))))
          (if (eq game_id -1)
              (setf game_id (parse-integer (nth 1 pair)))
              ())
          (if (string= "red" (nth 1 pair))
              (if (> (parse-integer (nth 0 pair)) red)
                  (return-from check-game-validity 0)
                  (setf total_r (+ total_r (parse-integer (nth 0 pair)))))
              ())
          (if (string= "blue" (nth 1 pair))
              (if (> (parse-integer (nth 0 pair)) blue)
                  (return-from check-game-validity 0)
                  (setf total_b (+ total_b (parse-integer (nth 0 pair)))))
              ())
          (if (string= "green" (nth 1 pair))
              (if (> (parse-integer (nth 0 pair)) green)
                  (return-from check-game-validity 0)
                  (setf total_g (+ total_g (parse-integer (nth 0 pair)))))
              ())))
    game_id
    ))

;; For part 2
(defun get-power (tokens)
  (let ((total_r 0)
        (total_g 0)
        (total_b 0)
        (game_id -1))
      (loop for token in tokens do
        (let ((pair (parse-token (string-trim " " token))))
          (if (eq game_id -1)
              (setf game_id (parse-integer (nth 1 pair)))
              ())
          (if (string= "red" (nth 1 pair))
              (setf total_r (max total_r (parse-integer (nth 0 pair))))
              ())
          (if (string= "green" (nth 1 pair))
              (setf total_g (max total_g (parse-integer (nth 0 pair))))
              ())
          (if (string= "blue" (nth 1 pair))
              (setf total_b (max total_b (parse-integer (nth 0 pair))))
              ())))
    (* total_r (* total_g total_b))
))

(defun parse-line (stream-in red green blue)
  (let ((line (read-line stream-in nil)))
    (if (null line)
        0
        (+
         (check-game-validity (cl-ppcre:split ",|;|:" line) red green blue)
         (parse-line stream-in red green blue)))
    ))

(defun parse-line-2 (stream-in)
  (let ((line (read-line stream-in nil)))
    (if (null line)
        0
        (+
         (get-power (cl-ppcre:split ",|;|:" line))
         (parse-line-2 stream-in)))
    ))

(defun compute-result (file red green blue)
  (with-open-file (file-stream file)
        (parse-line file-stream red green blue)))

(defun compute-result-2 (file)
  (with-open-file (file-stream file)
        (parse-line-2 file-stream)))

(format T "Day 2 part 1: ~d~%" (compute-result "day2.txt" 12 13 14))
(format T "Day 2 part 2: ~d~%" (compute-result-2 "day2.txt"))
